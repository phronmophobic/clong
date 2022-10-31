(ns com.phronemophobic.clong
  (:require [libpython-clj2.python :refer [py. py.. py.-] :as py]
            [zippo.core :as zippo]
            [clojure.string :as str]
            [tech.v3.datatype.ffi.clang :as ffi-clang]
            [tech.v3.datatype.ffi :as dt-ffi]
            [tech.v3.datatype.struct :as dt-struct]
            [com.rpl.specter :as specter]
            [clojure.java.io :as io]
            [loom.graph :as g]
            loom.alg
            [clojure.zip :as z]
            ))

;; Requires Java17

(py/initialize! :python-executable "/Users/adrian/cpython/out/bin/python3"
                :library-path "/Users/adrian/cpython/out/lib/libpython3.12.dylib")

;; must be required after initialize
(require '[libpython-clj2.require :refer [require-python]])

(require-python '[clang.cindex :as cl])

(defn iter->seq [py-iter]
  (lazy-seq
   (let [x (try
             (py. py-iter __next__)
             (catch Exception e
               (if (= "StopIteration\n" (ex-message e))
                 ::end
                 (throw e))))]
     (when (not= x ::end)
       (cons x
             (iter->seq py-iter))))))

(defn cursor-zip [cursor]
  (z/zipper #(seq (py. % get_children))
            #(py. % get_children)
            (fn [node childs]
              node)
            cursor))


(defn write-edn [w obj]
  (binding [*print-length* nil
            *print-level* nil
            *print-dup* false
            *print-meta* false
            *print-readably* true

            ;; namespaced maps not part of edn spec
            *print-namespace-maps* false

            *out* w]
    (pr obj)))

(defn struct-type->id [type]
  (let [struct-name (-> type
                        (py. get_canonical)
                        (py.- spelling))
        struct-name (cond

                      (str/starts-with? struct-name "struct ")
                      (subs struct-name (count "struct "))

                      (str/starts-with? struct-name "const ")
                      (subs struct-name (count "const "))
                      
                      :else struct-name)]
    (keyword "clang" struct-name)))

(defn coffi-integer-type [size]
  (case size
    1 :coffi.mem/char
    2 :coffi.mem/short
    4 :coffi.mem/int
    8 :coffi.mem/long))

(defn coffi-float-type [size]
  (case size
    4 :coffi.mem/float
    8 :coffi.mem/double))

(defn clang-type->coffi [type]
  (let [type (py. type get_canonical)]
    (condp = (py.- type kind)
      
      (py.- cl/TypeKind VOID) :coffi.mem/void
      (py.- cl/TypeKind CONSTANTARRAY) [:coffi.mem/array 
                                        (clang-type->coffi
                                         (py. type get_array_element_type))
                                        (py. type get_array_size)]
      ;; (py.- cl/TypeKind BOOL)
      ;; (py.- cl/TypeKind CHAR_U)
      ;; (py.- cl/TypeKind UCHAR)
      ;; (py.- cl/TypeKind CHAR16)
      ;; (py.- cl/TypeKind CHAR32)
      (py.- cl/TypeKind USHORT) (coffi-integer-type (py. type get_size))
      (py.- cl/TypeKind UINT) (coffi-integer-type (py. type get_size))
      (py.- cl/TypeKind ULONG) (coffi-integer-type (py. type get_size))
      (py.- cl/TypeKind ULONGLONG) (coffi-integer-type (py. type get_size))
      (py.- cl/TypeKind UINT128) (coffi-integer-type (py. type get_size))
      (py.- cl/TypeKind CHAR_S) (coffi-integer-type (py. type get_size))
      ;; (py.- cl/TypeKind SCHAR)
      ;; (py.- cl/TypeKind WCHAR) 
      (py.- cl/TypeKind SHORT) (coffi-integer-type (py. type get_size))
      (py.- cl/TypeKind INT) (coffi-integer-type (py. type get_size))
      (py.- cl/TypeKind LONG) (coffi-integer-type (py. type get_size))
      (py.- cl/TypeKind LONGLONG) (coffi-integer-type (py. type get_size))
      (py.- cl/TypeKind INT128) (coffi-integer-type (py. type get_size))

      
      (py.- cl/TypeKind FLOAT) (coffi-float-type (py. type get_size))
      (py.- cl/TypeKind DOUBLE) (coffi-float-type (py. type get_size))
      (py.- cl/TypeKind LONGDOUBLE) (coffi-float-type (py. type get_size))
      (py.- cl/TypeKind FLOAT128) (coffi-float-type (py. type get_size))
      ;; (py.- cl/TypeKind NULLPTR) 
      
      ;; (py.- cl/TypeKind COMPLEX)
      (py.- cl/TypeKind POINTER) (let [pointee-type (py. type get_pointee)
                                       pointee-type-kind (-> pointee-type
                                                             (py. get_canonical)
                                                             (py.- kind)) ]
                                   (cond
                                     (= (py.- cl/TypeKind VOID)
                                        pointee-type-kind)
                                     :coffi.mem/pointer
                                     
                                     (= (py.- cl/TypeKind FUNCTIONPROTO)
                                        pointee-type-kind)
                                     [:coffi.ffi/fn
                                      (mapv clang-type->coffi
                                            (py. pointee-type argument_types))
                                      (clang-type->coffi
                                       (py. pointee-type get_result))]

                                     :else
                                     [:coffi.mem/pointer
                                      (clang-type->coffi pointee-type)]))

      ;; (py.- cl/TypeKind FUNCTIONPROTO) 

      ;; objc block function pointer?
      (py.- cl/TypeKind BLOCKPOINTER) [:coffi.mem/pointer]
      
      (py.- cl/TypeKind RECORD) (struct-type->id type)
      (py.- cl/TypeKind ENUM) (coffi-integer-type (py. type get_size)))))


(def function-decl-kind (py/get-attr cl/CursorKind "FUNCTION_DECL"))

(defn function-decl? [cursor]
  (= function-decl-kind (py/get-attr cursor "kind")))

(defn fdecl->map [cursor]
  (let [fname (py.- cursor spelling)
        ret-type (py.- cursor result_type)
        ret {:spelling (py.- ret-type spelling)
             :type (-> ret-type
                       (py. get_canonical)
                       (py.- spelling))}
        
        args
        (for [arg (py. cursor get_arguments)]
          {:spelling (py.- arg spelling)
           :type (-> arg
                     (py.- type)
                     (py. get_canonical)
                     (py.- spelling))})
        doc (py.- cursor raw_comment)]
    {:id (keyword fname)
     :type :function
     :symbol fname
     :function/args
     (into []
           (comp (map (fn [arg]
                        (py.- arg type)))
                 (map clang-type->coffi))
           (py. cursor get_arguments))
     :function/ret (clang-type->coffi
                    ret-type)
     :args args
    :ret ret
     :linkage (py.- cursor linkage)
     :name fname
     :doc doc}))


(def macro-decl-kind (py/get-attr cl/CursorKind "MACRO_DEFINITION"))
(defn macro-decl? [cursor]
  (=  (py.- cursor kind)
      macro-decl-kind))


(def struct-decl-kind (py/get-attr cl/CursorKind "STRUCT_DECL"))

(defn struct-decl? [cursor]
  (= struct-decl-kind (py/get-attr cursor "kind")))


(def type-kind-pointer (py.- cl/TypeKind POINTER)) 
(defn pointer-type? [type]
  (= type-kind-pointer
     (py.- type kind)))

(defn num-type [kw type]
  ;; get_size returns size in bytes
  (keyword (str (name kw) (* 8 (py. type get_size)))))

(defn array-type? [type]
  (let [type (py. type get_canonical)]
    (= (py.- cl/TypeKind CONSTANTARRAY)
       (py.- type kind))))



(defn clang-type->dtype [type]
  (let [type (py. type get_canonical)]
    (condp = (py.- type kind)
      
      (py.- cl/TypeKind VOID) :void
      (py.- cl/TypeKind CONSTANTARRAY) (clang-type->dtype
                                        (py. type get_array_element_type))
      ;; (py.- cl/TypeKind BOOL)
      ;; (py.- cl/TypeKind CHAR_U)
      ;; (py.- cl/TypeKind UCHAR)
      ;; (py.- cl/TypeKind CHAR16)
      ;; (py.- cl/TypeKind CHAR32)
      (py.- cl/TypeKind USHORT) (num-type :int type)
      (py.- cl/TypeKind UINT) (num-type :int type)
      (py.- cl/TypeKind ULONG) (num-type :int type)
      (py.- cl/TypeKind ULONGLONG) (num-type :int type)
      (py.- cl/TypeKind UINT128) (num-type :int type)
      ;; (py.- cl/TypeKind CHAR_S)
      ;; (py.- cl/TypeKind SCHAR)
      ;; (py.- cl/TypeKind WCHAR) 
      (py.- cl/TypeKind SHORT) (num-type :int type)
      (py.- cl/TypeKind INT) (num-type :int type)
      (py.- cl/TypeKind LONG) (num-type :int type)
      (py.- cl/TypeKind LONGLONG) (num-type :int type)
      ;; (py.- cl/TypeKind INT128) 
      (py.- cl/TypeKind FLOAT) (num-type :float type)
      (py.- cl/TypeKind DOUBLE) (num-type :float type)
      (py.- cl/TypeKind LONGDOUBLE) (num-type :float type)
      ;; (py.- cl/TypeKind NULLPTR) 
      (py.- cl/TypeKind FLOAT128) (num-type :float type)
      
      ;; (py.- cl/TypeKind COMPLEX)
      ;; (py.- cl/TypeKind POINTER) :pointer
      (py.- cl/TypeKind POINTER) (num-type :int type)

      ;; function pointer?
      (py.- cl/TypeKind BLOCKPOINTER) (num-type :int type)
      
      (py.- cl/TypeKind RECORD) (struct-type->id type)
      (py.- cl/TypeKind ENUM) :int32)))





(defn struct-type->map [cursor]
  (let [stype (-> cursor
                  (py. get_canonical))
        fields (py. stype get_fields)]
    (for [field fields
          :let [field-type (py.- field type)]]
      {:name (py.- field spelling)
       :datatype (clang-type->coffi field-type)
       :n-elems (if (array-type? field-type)
                  (py. field-type get_array_size)
                  1)
       :calculated-offset (py. field get_field_offsetof)})))

(defn struct-decl->map [cursor]
  (let [type (py.- cursor type)
        fields (struct-type->map type)]
    {:id (struct-type->id type)
     :size-in-bytes (py. type get_size)
     :fields fields}))

(def enum-decl-kind (py/get-attr cl/CursorKind "ENUM_CONSTANT_DECL"))

(defn enum-decl? [cursor]
  (= enum-decl-kind (py/get-attr cursor "kind")))


(def external-linkage (py.- cl/LinkageKind EXTERNAL))

(defn get-cursor-tree [fname args]
  (let [parse-detailed-opt (-> cl/TranslationUnit
                               (py.- PARSE_DETAILED_PROCESSING_RECORD))
        index (py. cl/Index create)
        translation-unit (py/py** index parse
                                  fname
                                  args
                                  {"options" parse-detailed-opt})
        cursor (py/get-attr translation-unit "cursor")
        tree (tree-seq (constantly true)
                       #(py. % get_children)
                       cursor)]
    tree))
(comment
  (def tree (get-cursor-tree
             "/Users/adrian/workspace/llvm-project/build/out/include/clang-c/Index.h"
             ["-I/Users/adrian/workspace/llvm-project/build/out/include/"]))
  ,)

(defn parse-api [fname args]
  (let [parse-detailed-opt (-> cl/TranslationUnit
                               (py.- PARSE_DETAILED_PROCESSING_RECORD))
        index (py. cl/Index create)
        translation-unit (py/py** index parse
                                  fname
                                  args
                                  {"options" parse-detailed-opt})
        cursor (py/get-attr translation-unit "cursor")
        tree (tree-seq (constantly true)
                       #(py. % get_children)
                       cursor)

        struct-decls
        (doall
         (->> tree
              (filter struct-decl?)
              (map struct-decl->map)
              (into {}
                    (map (fn [m]
                           [(:id m)
                            m])))))

        ;; sort topologically
        struct-g
        (apply g/digraph
               (eduction
                (mapcat (fn [m]
                          (map (fn [field]
                                 [(:datatype field) (:id m)])
                               (:fields m))))
                (vals struct-decls)))
        structs (->> (loom.alg/topsort struct-g)
                     (keep struct-decls))

        function-decls
        (doall
         (->> tree
              (filter function-decl?)
              (map fdecl->map)
              (filter #(= external-linkage (:linkage %)))))

        enum-decls
        (doall
         (->> tree
              (filter enum-decl?)
              (map (fn [cur]
                     {:name (py.- cur spelling)
                      :doc (py.- cur raw_comment)
                      :value (py.- cur enum_value)}))))]
    {:functions function-decls
     :structs structs
     :enums enum-decls}))







(defn type->fn-type [kw]
  (let [nm (name kw)]
    (cond
     (str/starts-with? nm "uint")
     (keyword (str "int" (subs nm (count "uint"))))

     :else kw))
  )


(defn dump-clang-api []
  (with-open [w (io/writer (io/file
                            "resources" "clang-api.edn"))]
    (write-edn w (parse-api
                  "/Users/adrian/workspace/llvm-project/build/out/include/clang-c/Index.h"
                  ["-I/Users/adrian/workspace/llvm-project/build/out/include/"]))))





(ns com.phronemophobic.clong.clang
  (:require [com.phronemophobic.clong.clang.jna.raw :as c]
            [clojure.string :as str]
            [loom.graph :as g]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            loom.alg))


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

;; can find by calling clang -### empty-file.h
(def default-arguments
  ["-resource-dir"
   "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/13.1.6"
   "-isysroot"
   "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk"
   "-I"
   "/opt/local/include/"
   "-I/usr/local/include"
   "-internal-isystem"
   "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/local/include"
   "-internal-isystem"
   "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/13.1.6/include"
   "-internal-externc-isystem"
   "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include"
   "-internal-externc-isystem"
   "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
   "-syslibroot"
   "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk"])

(defn struct-type->id [type]
  (let [struct-name (-> type
                        (c/clang_getCanonicalType)
                        (c/clang_getTypeSpelling)
                        (c/clang_getCString))
        struct-name (cond

                      (str/starts-with? struct-name "struct ")
                      (subs struct-name (count "struct "))

                      (str/starts-with? struct-name "const ")
                      (subs struct-name (count "const "))

                      :else struct-name)]
    (keyword "clong" struct-name)))

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

(defn get-argument-types [type]
  (into []
        (map (fn [i]
               (c/clang_getArgType type i)))
        (range (c/clang_getNumArgTypes type))))

(defn clang-type->coffi [type]

  (let [type (c/clang_getCanonicalType type)]
    #_(prn (c/clang_getCString
          (c/clang_getTypeSpelling type)))
    (condp = (.kind type)

      c/CXType_Void :coffi.mem/void
      c/CXType_ConstantArray [:coffi.mem/array
                              (clang-type->coffi
                               (c/clang_getArrayElementType type))
                              (c/clang_getArraySize type)]
      ;; c/CXType_Bool
      ;; c/CXType_Char_U
      ;; c/CXType_UChar
      ;; c/CXType_Char16
      ;; c/CXType_Char32

      c/CXType_UShort (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_UInt (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_ULong (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_ULongLong (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_UInt128 (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_Char_S (coffi-integer-type (c/clang_Type_getSizeOf type))
      ;; c/CXType_SChar
      ;; c/CXType_WChar)
      c/CXType_Short (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_Int (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_Long (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_LongLong (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_Int128 (coffi-integer-type (c/clang_Type_getSizeOf type))

      c/CXType_Float (coffi-float-type (c/clang_Type_getSizeOf type))
      c/CXType_Double (coffi-float-type (c/clang_Type_getSizeOf type))
      c/CXType_LongDouble (coffi-float-type (c/clang_Type_getSizeOf type))
      c/CXType_Float128 (coffi-float-type (c/clang_Type_getSizeOf type))
      ;; c/CXType_NullPtr
      ;; c/CXType_Complex

      c/CXType_Pointer (let [pointee-type (-> (c/clang_getPointeeType type)
                                              (c/clang_getCanonicalType))
                             pointee-type-kind (-> pointee-type
                                                   (.kind)) ]
                         (cond
                           (= c/CXType_Void
                              pointee-type-kind)
                           :coffi.mem/pointer

                           (= c/CXType_FunctionProto
                              pointee-type-kind)
                           [:coffi.ffi/fn
                            (mapv clang-type->coffi
                                  (get-argument-types pointee-type))
                            (clang-type->coffi (c/clang_getResultType pointee-type))]

                           :else
                           [:coffi.mem/pointer
                            (clang-type->coffi pointee-type)]))

      ;; c/CXType_FunctionProto

      ;; objc block function pointer?
      c/CXType_BlockPointer [:coffi.mem/pointer]

      c/CXType_Record (struct-type->id type)
      c/CXType_Enum (coffi-integer-type (c/clang_Type_getSizeOf type)))))


(defonce handles (atom #{}))
(defn ref! [o]
  (swap! handles conj o)
  o)
(defn parse [fname args]
  (let [

        index (ref! (c/clang_createIndex 0 0))

        options c/CXTranslationUnit_DetailedPreprocessingRecord
        translation-unit (c/clang_parseTranslationUnit index
                                                       (ref! fname)
                                                       (ref!
                                                        (into-array String
                                                                    args))
                                                       (count args)
                                                       nil
                                                       0
                                                       options)
        cursor (ref!
                (c/clang_getTranslationUnitCursor translation-unit))
        ]

    cursor))

(defn get-children [cursor]

  (let [childs (volatile! [])
        visitor (ref!
                 (fn [child parent _]
                   (vswap! childs conj child)
                   c/CXChildVisit_Recurse))]
    (c/clang_visitChildren cursor
                           visitor
                           nil)
    (ref! @childs)
    @childs))

(def ^:private cursor-kinds*
  (->> (:enums c/clang-api)
       (filter #(= "CXCursorKind"
                   (:enum %)))
       (into {}
             (map (fn [e]
                    [(:value e)
                     (:name e)])))))

(defn cursor-kind->str [n]
  (cursor-kinds* n))

(def ^:private cursor-type-kinds*
  (->> (:enums c/clang-api)
       (filter #(= "CXTypeKind"
                   (:enum %)))
       (into {}
             (map (fn [e]
                    [(:value e)
                     (:name e)])))))

(defn cursor-type-kind->str [n]
  (cursor-type-kinds* n))


(def ^:private linkage-type-kinds*
  (->> (:enums c/clang-api)
       (filter #(= "CXLinkageKind"
                   (:enum %)))
       (into {}
             (map (fn [e]
                    [(:value e)
                     (:name e)])))))

(defn linkage-kind->str [n]
  (linkage-type-kinds* n))

(defmulti extra-cursor-info (fn [cur]
                              (cursor-kind->str (.kind cur))))


(defmethod extra-cursor-info :default
  [cur]
  {})

(defn cursor-info [cur]
  (merge
   {:kind (cursor-kind->str (.kind cur))
    :type (cursor-type-kind->str (.kind (ref!
                                         (c/clang_getCursorType cur))))}
   (extra-cursor-info cur)))


(defn enum-decl? [cur]
  (= "CXCursor_EnumConstantDecl"
     (:kind cur)))

(defmethod extra-cursor-info "CXCursor_EnumConstantDecl"
  [cur]
  {:name (c/clang_getCString
          (c/clang_getCursorSpelling cur))
   :value (-> cur
              c/clang_getEnumConstantDeclValue)
   :enum (-> cur
             c/clang_getCursorSemanticParent
             c/clang_getCursorSpelling
             c/clang_getCString)
   :raw-comment (c/clang_getCString
                 (c/clang_Cursor_getRawCommentText cur))})

(defn function-decl? [cur]
  (= "CXCursor_FunctionDecl"
     (:kind cur)))

(defmethod extra-cursor-info "CXCursor_FunctionDecl"
  [cur]
  (def my-cur cur)
  (let [fname (c/clang_getCString
               (c/clang_getCursorSpelling cur))
        num-arguments (c/clang_Cursor_getNumArguments cur)]
    {:id (keyword fname)
     :symbol fname
     :args (into []
                 (comp
                  (map (fn [i]
                         (c/clang_Cursor_getArgument cur i)))
                  (map (fn [arg]
                         {:spelling (-> arg
                                        c/clang_getCursorSpelling
                                        c/clang_getCString)
                          :type (-> arg
                                    c/clang_getCursorType
                                    c/clang_getCanonicalType
                                    c/clang_getTypeSpelling
                                    c/clang_getCString)})))
                 (range num-arguments))
     :ret {:spelling (-> (c/clang_getCursorResultType cur)
                         c/clang_getCanonicalType
                         c/clang_getTypeSpelling
                         c/clang_getCString)}
     :function/args
     (into []
           (comp
            (map (fn [i]
                   (c/clang_Cursor_getArgument cur i)))
            (map c/clang_getCursorType)
            (map clang-type->coffi))

           (range num-arguments))
     :linkage (linkage-kind->str (c/clang_getCursorLinkage cur))
     :function/ret (-> cur
                       (c/clang_getCursorResultType)
                       clang-type->coffi)
     :raw-comment (c/clang_getCString
                   (c/clang_Cursor_getRawCommentText cur))}))

(defn struct-decl? [cur]
  (= "CXCursor_StructDecl"
     (:kind cur)))


(defn get-fields [field-type]
  (let [childs (volatile! [])
        visitor (ref!
                 (fn [child _]
                   (vswap! childs conj child)
                   c/CXChildVisit_Recurse))]
    (c/clang_Type_visitFields field-type
                              visitor
                              nil)
    (ref! @childs)
    @childs))

(defn field->map [cur]
  (let [field-type (-> cur
                       (c/clang_getCursorType)
                       (c/clang_getCanonicalType))
        spelling (c/clang_getCString
                  (c/clang_getCursorSpelling cur))
        type-spelling (c/clang_getCString
                       (c/clang_getTypeSpelling field-type))]
    {:type type-spelling
     :datatype (clang-type->coffi field-type)
     :name spelling
     :calculated-offset (c/clang_Cursor_getOffsetOfField cur)})
  )

(defmethod extra-cursor-info "CXCursor_StructDecl"
  [cur]

  (let [
        num-arguments (c/clang_Cursor_getNumArguments cur)
        stype (-> (c/clang_getCursorType cur)
                  c/clang_getCanonicalType)
        spelling (c/clang_getCString
                  (c/clang_getTypeSpelling stype))]
    {
     :id (struct-type->id stype)
     :spelling spelling
     :size-in-bytes (c/clang_Type_getSizeOf stype)
     :fields (mapv field->map (get-fields stype))}))

 (defn gen-api [root]
  (let [tree (->> (get-children root)
                  (map cursor-info)
                  doall)

        enums (->> tree
                   (filter enum-decl?)
                   doall)

        structs-by-id (->> tree
                           (filter struct-decl?)
                           (into {}
                                 (map (fn [m]
                                        [(:id m)
                                         m]))))



        ;; sort topologically
        struct-g
        (apply g/digraph
               (concat
                ;; nodes
                (keys structs-by-id)
                ;; edges
                (eduction
                 (mapcat (fn [m]
                           (keep (fn [field]
                                   (when (contains? structs-by-id
                                                    (:datatype field))
                                     [(:datatype field)  (:id m)]))
                                 (:fields m))))
                 (vals structs-by-id))))

        structs (->> (loom.alg/topsort struct-g)
                     (keep structs-by-id))

        functions (->> tree
                 (filter function-decl?)
                 (filter #(= "CXLinkage_External"
                             (:linkage %)))
                 doall)

]
    {:functions functions
     :structs structs
     :enums enums}))


(defn dump-clang-api []
  (with-open [w (io/writer (io/file
                            "resources" "clang-api.edn"))]
    (write-edn w (gen-api
                  (parse "/Users/adrian/workspace/llvm-project/build/out/include/clang-c/Index.h"
                         ["-I/Users/adrian/workspace/llvm-project/build/out/include/"])))))

(comment

  (def cursor (parse
               "/Users/adrian/workspace/llvm-project/build/out/include/clang-c/Index.h"
               ["-I/Users/adrian/workspace/llvm-project/build/out/include/"]))

  #_(def cursor (parse
               "/Users/adrian/workspace/clong/csource/align.c"
               ["-I/Users/adrian/workspace/llvm-project/build/out/include/"]

               ))

  (dotimes [i 1000]
    (c/clang_getCursorType cursor))

  (def cursors (get-children cursor))

  (def tree (->> (get-children cursor)
                 (map cursor-info)))
  

  (def structs (->> tree
                    (filter struct-decl?)))

  (def structs-by-id
    (into {}
          (map (juxt :id identity))
          structs))
  


  (def fns (->> tree
                (filter function-decl?)))

  (def fns-by-id
    (into {}
          (map (juxt :id identity))
          fns))

  (def enums
    (->> tree
         (filter enum-decl?)))


  (def enums-by-id
    (into {}
          (map (juxt :name identity))
          enums))
  
  ,)

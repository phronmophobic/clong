(ns com.phronemophobic.clong.clang
  (:require [com.phronemophobic.clong.clang.jna.raw :as c]
            [clojure.string :as str]
            [loom.graph :as g]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            loom.alg)
  (:import java.io.PushbackReader
           com.sun.jna.ptr.FloatByReference
           com.sun.jna.ptr.IntByReference
           com.sun.jna.ptr.PointerByReference))


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
  [ "-resource-dir"
   "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/15.0.0"
   "-isysroot"
   "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk"
   "-I/usr/local/include"
   "-internal-isystem"
   "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/local/include"
   "-internal-isystem"
   "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/15.0.0/include"
   "-internal-externc-isystem"
   "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
   "-internal-externc-isystem"
   "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
   ])

(defn get-spelling-location [location]
  (let [filep (PointerByReference.)
        line (IntByReference.)
        col (IntByReference.)
        offset (IntByReference.)]
    (c/clang_getSpellingLocation location filep line col offset)
    (let [filename (-> (c/clang_getFileName (.getValue filep))
                       (c/clang_getCString))]
      {:line (.getValue line)
       :file filename
       :col (.getValue col)
       :offset (.getValue offset)})))

(defn get-spelling-extent [cur]
  (let [extent (c/clang_getCursorExtent cur)
        start-loc (-> extent
                      c/clang_getRangeStart
                      get-spelling-location)
        end-loc (-> extent
                    c/clang_getRangeStart
                    get-spelling-location)]
    {:file (:file start-loc)
     :start start-loc
     :end end-loc}))


(declare field->map get-fields)
(defn struct-type->id [type]
  (let [cur (c/clang_getTypeDeclaration type)
        anonymous? (not (zero? (c/clang_Cursor_isAnonymous cur)))]
    (if anonymous?
      ;; should use clang_getCursorUSR?
      ;; negative hash numbers add hypens
      (keyword "clong" (str "Struct_" (format "%X" (hash (mapv field->map (get-fields type)) ))))
      (let [struct-name (-> type
                            (c/clang_getCanonicalType)
                            (c/clang_getTypeSpelling)
                            (c/clang_getCString))
            orig struct-name
            struct-name (if (str/starts-with? struct-name "const ")
                          (subs struct-name (count "const "))
                          struct-name)

            struct-name (if (str/starts-with? struct-name "struct ")
                          (subs struct-name (count "struct "))
                          struct-name)

            ;; ;; enforce starting with a letter?
            ;; struct-name (if (not (re-find #"^[a-zA-Z]" struct-name))
            ;;               (str ))
            ]
        (keyword "clong" struct-name)))))

(defn union-type? [t]
  (let [t (c/clang_getCanonicalType t)
        cur (c/clang_getTypeDeclaration t)]
    (= c/CXCursor_UnionDecl
       (:kind cur))))

(defn coffi-integer-type [size]
  (case size
    1 :coffi.mem/char
    2 :coffi.mem/short
    4 :coffi.mem/int
    8 :coffi.mem/long
    ;; not sure what to call this
    16 :coffi.mem/longlong
    ))

(defn coffi-float-type [size]
  (case size
    4 :coffi.mem/float
    8 :coffi.mem/double
    16 [:coffi.mem/array :coffi.mem/double 2]))

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
      c/CXType_Bool (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_Char_U (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_UChar (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_Char16 (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_Char32 (coffi-integer-type (c/clang_Type_getSizeOf type))

      c/CXType_UShort (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_UInt (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_ULong (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_ULongLong (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_UInt128 (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_Char_S (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_SChar (coffi-integer-type (c/clang_Type_getSizeOf type))
      c/CXType_WChar (coffi-integer-type (c/clang_Type_getSizeOf type))
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

      ;; A type whose specific kind is not exposed via this interface
      c/CXType_Half :coffi.mem/type-half

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
                           (clang-type->coffi pointee-type)

                           :else
                           [:coffi.mem/pointer
                            (clang-type->coffi pointee-type)]))

      c/CXType_FunctionProto
      [:coffi.ffi/fn
       (mapv clang-type->coffi
             (get-argument-types type))
       (clang-type->coffi (c/clang_getResultType type))]

      c/CXType_IncompleteArray
      :coffi.mem/pointer

      ;; objc block function pointer?
      c/CXType_BlockPointer [:coffi.mem/pointer]

      c/CXType_Record
      (if (union-type? type)
        ;; pretend for now
        [:coffi.mem/array :coffi.mem/char (c/clang_Type_getSizeOf type)]
        (struct-type->id type))

      c/CXType_Enum :coffi.mem/int

      )))


(defonce handles (atom #{}))
(defn ref! [o]
  (swap! handles conj o)
  o)
(defn parse
  "Returns a CXCursor.

  Further processing can be done via the raw api  in com.phronemophobic.clong.clang.jna.raw. For basic usage, just use:
  (->> (parse \"myheader.h\" default-arguments)
       get-children
       (map cursor-info))
"
  [fname args]
  (let [

        index (ref! (c/clang_createIndex 0 0))

        options c/CXTranslationUnit_DetailedPreprocessingRecord

        translation-unit* (PointerByReference.)
        err
        (c/clang_parseTranslationUnit2 index
                                       (ref! fname)
                                       (ref!
                                        (into-array String
                                                    args))
                                       (count args)
                                       nil
                                       0
                                       options
                                       translation-unit*)
        _ (when (not (zero? err))
            (throw (ex-info "Parse Error"
                            {:error-code err})))

        translation-unit (.getValue translation-unit*)
        _ (assert translation-unit)

        cursor (ref!
                (c/clang_getTranslationUnitCursor translation-unit))]

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

(defn get-immediate-children [cursor]

  (let [childs (volatile! [])
        visitor (ref!
                 (fn [child parent _]
                   (vswap! childs conj child)
                   c/CXChildVisit_Continue))]
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
  ;; (prn (:file (get-spelling-extent cur)))
  (merge
   {:kind (cursor-kind->str (.kind cur))
    :spelling (c/clang_getCString
               (c/clang_getCursorSpelling cur))
    :cur cur
    :location (get-spelling-extent cur)
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

(defonce get-source (memoize slurp))
(defn get-snippet [cur]
  (let [range (c/clang_getCursorExtent cur)
        start-location (get-spelling-location
                          (c/clang_getRangeStart range))
          end-location (get-spelling-location(c/clang_getRangeEnd range))
        snippet (when-let [filename (:file start-location)]
                  (let [source (get-source filename)]
                    (subs source
                          (:offset start-location)
                          (:offset end-location))))]
    snippet))



(comment
  (defmethod extra-cursor-info "CXCursor_MacroDefinition"
    [cur]
    (let [snippet (get-snippet cur)]
      {:spelling (c/clang_getCString
                  (c/clang_getCursorSpelling cur))
       :snippet snippet
       :definition (-> cur
                       (c/clang_getCursorDefinition)
                       (c/clang_getCursorSpelling )
                       (c/clang_getCString))
       :children (->> (get-immediate-children cur)
                      (map cursor-info))
       :type (-> cur
                 c/clang_getCursorType
                 c/clang_getCanonicalType
                 c/clang_getTypeSpelling
                 c/clang_getCString)}

      )))

(defmethod extra-cursor-info "CXCursor_FunctionDecl"
  [cur]
  (let [fname (c/clang_getCString
               (c/clang_getCursorSpelling cur))
        num-arguments (c/clang_Cursor_getNumArguments cur)]
    #_(prn fname)
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
                   c/CXChildVisit_Continue))]
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
                       (c/clang_getTypeSpelling field-type))
        bitfield? (not (zero? (c/clang_Cursor_isBitField cur)))
        m {:type type-spelling
           :datatype (clang-type->coffi field-type)
           :name spelling
           :bitfield? bitfield?
           :calculated-offset (c/clang_Cursor_getOffsetOfField cur)}]
    (if bitfield?
      (assoc m :bitfield-width (c/clang_getFieldDeclBitWidth cur))
      m)))

(defmethod extra-cursor-info "CXCursor_StructDecl"
  [cur]
  (let [
        num-arguments (c/clang_Cursor_getNumArguments cur)
        stype (-> (c/clang_getCursorType cur)
                  c/clang_getCanonicalType)

        spelling (c/clang_getCString
                  (c/clang_getTypeSpelling stype))
        anonymous (not (zero? (c/clang_Cursor_isAnonymous cur)))]
    #_(prn spelling (not (zero? (c/clang_equalCursors cur
                                                    (c/clang_getCanonicalCursor cur))))
         loc
         (mapv field->map (get-fields stype)))
    {:anonymous anonymous
     :id (struct-type->id stype)
     :spelling spelling
     :size-in-bytes (c/clang_Type_getSizeOf stype)
     :fields (mapv field->map (get-fields stype))}))

(def default-api-xforms
  (comp (map cursor-info)
        (remove (fn [cur]
                  (let [file (-> cur :location :file)]
                    (or (nil? file)
                        (str/starts-with? file
                                          "/Applications/Xcode.app")))))
        ;; remove forward declarations
        (remove (fn [cur]
                  (and (struct-decl? cur)
                       (empty? (:fields cur)))))))

(def enum-api-keys [:kind :spelling :type :name :value :enum :raw-comment])
(def struct-api-keys [:kind :spelling :type :id :size-in-bytes :fields])
(def function-api-keys [:args :ret :function/args :symbol :function/ret :type :linkage :id :raw-comment :kind :spelling])
(defn gen-api [cursors]
  (let [enums (->> cursors
                   (filter enum-decl?)
                   (map #(select-keys % enum-api-keys))
                   doall)

        structs-by-id (->> cursors
                           (filter struct-decl?)
                           (map #(select-keys % struct-api-keys))
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

        functions (->> cursors
                       (filter function-decl?)
                       (map #(select-keys % function-api-keys))
                       (filter #(= "CXLinkage_External"
                                   (:linkage %)))
                       doall)

        ]
    {:functions functions
     :structs structs
     :enums enums}))

(defn easy-api
  ([header]
   (easy-api header default-arguments))
  ([header args]
   (->> (parse header args)
        get-children
        (eduction default-api-xforms)
        gen-api)))

(defn dump-clang-api []
  (with-open [w (io/writer (io/file
                            "resources"
                            "com"
                            "phronemophobic"
                            "clong"
                            "clang"
                            "api.edn"))]
    (write-edn w (->> (parse "/Users/adrian/workspace/llvm-project/out/include/clang-c/Index.h"
                             (conj
                              default-arguments
                              "-I/Users/adrian/workspace/llvm-project/out/include/"))
                      get-children
                      (map cursor-info)
                      (remove (fn [cur]
                                (let [file (-> cur :location :file)]
                                  (or (nil? file)
                                      (str/starts-with? file
                                                        "/Applications/Xcode.app")))))
                      ;; remove forward declarations
                      (remove (fn [cur]
                                (and (struct-decl? cur)
                                     (empty? (:fields cur)))))
                      (gen-api)))))


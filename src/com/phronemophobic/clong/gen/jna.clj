(ns com.phronemophobic.clong.gen.jna
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [insn.core :as insn]
            [clojure.pprint :refer [pprint]]
            [insn.util :as insn-util]
            [com.rpl.specter :as specter]
            [clojure.edn :as edn])
  (:import java.io.PushbackReader
           com.sun.jna.Memory
           com.sun.jna.Pointer
           com.sun.jna.PointerType
           com.sun.jna.Platform
           com.sun.jna.ptr.ByteByReference
           com.sun.jna.ptr.ShortByReference
           com.sun.jna.ptr.IntByReference
           com.sun.jna.ptr.LongByReference
           com.sun.jna.ptr.FloatByReference
           com.sun.jna.ptr.DoubleByReference
           com.sun.jna.ptr.PointerByReference
           com.sun.jna.IntegerType
           com.sun.jna.Structure$ByValue
           com.sun.jna.Structure$ByReference
           com.sun.jna.Structure
           com.sun.jna.Structure$FieldOrder
           com.sun.jna.Callback
           java.util.List

           clojure.java.api.Clojure
           clojure.lang.IFn
           clojure.lang.ILookup))

(def ^:no-doc main-class-loader @clojure.lang.Compiler/LOADER)
(def ^:no-doc void Void/TYPE)

;; (defmacro ^:no-doc defc
;;   ([fn-name lib ret]
;;    `(defc ~fn-name ~lib ~ret []))
;;   ([fn-name lib ret args]
;;    (let [cfn-sym (with-meta (gensym "cfn") {:tag 'com.sun.jna.Function})]
;;      `(let [~cfn-sym (delay (.getFunction ~(with-meta `(deref ~lib) {:tag 'com.sun.jna.NativeLibrary})
;;                                           ~(name fn-name)))]
;;         (defn- ~fn-name [~@args]
;;           (.invoke (deref ~cfn-sym)
;;                    ~ret (to-array [~@args])))))))

;; ;; (defc dispatch_sync_f objlib void [queue context work])

(defn coffi-type->insn-type [struct-prefix t]
  (case t
    :coffi.mem/char :byte
    :coffi.mem/short :short
    :coffi.mem/int :int
    :coffi.mem/long :long
    :coffi.mem/float :float    
    :coffi.mem/double :double
    :coffi.mem/pointer Pointer
    :coffi.mem/void :void

    (cond
      (vector? t)
      (case (first t)
        :coffi.mem/pointer
        (let [ptype (second t)]
          (case ptype
            :coffi.mem/char ByteByReference
            :coffi.mem/short ShortByReference
            :coffi.mem/int IntByReference
            :coffi.mem/long LongByReference
            :coffi.mem/float FloatByReference
            :coffi.mem/double DoubleByReference
            :coffi.mem/pointer PointerByReference

            ;; else
            (if-not (keyword ptype)
              Pointer
              (cond
                (= ptype :coffi.mem/char)
                String

                (not= "coffi.mem"
                      (namespace ptype))
                (reify
                  insn-util/ClassDesc
                  (class-desc [_]
                    (str (str/replace struct-prefix #"\." "/") "/" (name ptype) "ByReference"))
                  insn-util/TypeDesc
                  (type-desc [_]
                    (str "L" (str/replace struct-prefix #"\." "/") "/" (name ptype) "ByReference;")))

                :else Pointer))))


        :coffi.ffi/fn Pointer
        ;; Callback is abstract. Need specific type info to be useful
        ;;com.sun.jna.Callback
        
        :coffi.mem/array
        [(coffi-type->insn-type struct-prefix (second t))])
      

      (keyword? t)
      (if (= "coffi.mem" (namespace t))
        (throw (ex-info "Unknown coffi type."
                        {:t t}))
        ;;else
        (reify
          insn-util/ClassDesc
          (class-desc [_]
            (str (str/replace struct-prefix #"\." "/") "/" (name t)))
          insn-util/TypeDesc
          (type-desc [_]
            (str "L" (str/replace struct-prefix #"\." "/") "/" (name t) ";")))))))

(defn coffi-type->jna [struct-prefix t]
  (case t
    :coffi.mem/char Byte/TYPE
    :coffi.mem/short Short/TYPE
    :coffi.mem/int Integer/TYPE
    :coffi.mem/long Long/TYPE
    :coffi.mem/float Float/TYPE    
    :coffi.mem/double Double/TYPE
    :coffi.mem/pointer Pointer
    :coffi.mem/void void
    
    (cond
      (= [:coffi.mem/pointer :coffi.mem/char]
         t)
      String

      (vector? t)
      (case (first t)
        :coffi.mem/pointer
        (let [ptype (second t)]
          (case ptype
            :coffi.mem/char ByteByReference
            :coffi.mem/short ShortByReference
            :coffi.mem/int IntByReference
            :coffi.mem/long LongByReference
            :coffi.mem/float FloatByReference
            :coffi.mem/double DoubleByReference
            :coffi.mem/pointer PointerByReference

            ;; else
            (if-not (keyword ptype)
              Pointer
              (cond
                (= ptype :coffi.mem/char)
                String

                (not= "coffi.mem"
                      (namespace ptype))
                (try
                  (Class/forName(str struct-prefix "." (name ptype) "ByReference"))
                  (catch java.lang.ClassNotFoundException e
                    Pointer))

                :else Pointer))))

        :coffi.ffi/fn Pointer
        ;;com.sun.jna.Callback
        
        :coffi.mem/array
        (Class/forName (insn-util/type-desc [(coffi-type->insn-type struct-prefix (second t))])))

      (keyword? t)
      (if (= "coffi.mem" (namespace t))
        (throw (ex-info "Unknown coffi type."
                        {:t t}))
        ;;else
        (Class/forName
         (str struct-prefix "." (name t)))))))

(defonce ^:no-doc not-garbage
  (atom []))

(defn ^:no-doc preserve!
  "Store this value so it's not garbage collected"
  [x]
  (swap! not-garbage conj x)
  x)

 (defn load-var-inst [sym]
   [[:ldc (namespace sym)]
    [:ldc (name sym)]
    [:invokestatic Clojure "var" [Object Object IFn]]])

(defn ^:private structure_valAt
  ([s k]
   (.readField ^Structure s (name k))))

(defn field-name [field]
  (let [fname (:name field)]
    ;; sometimes, field names are empty
    ;; generate a consistent, automatic name
    ;; if there is a conflict, class creation will
    ;;   throw an error
    (if (= fname "")
      (str "__anonymous_field_" (:calculated-offset field))
      fname)))

(defn struct->class-def* [struct-prefix struct]
  (let [fields (:fields struct)
        has-bitfields? (some :bitfield? fields)
        ;; jna doesn't support bitfields
        ;; https://github.com/java-native-access/jna/issues/423
        ;; for now, just replace all fields with an opaque
        ;; field so that the structure is the right size
        ;; and we don't have fields pointing to the wrong data
        fields (if has-bitfields?
                 [{:name "opaque__no_bitfield_support_yet"
                   :datatype [:coffi.mem/array :coffi.mem/char
                              (:size-in-bytes struct)]}]
                 fields)]
    {
     ;; :name (symbol (str struct-prefix "." (name (:id struct))))
     ;; :interfaces [Structure$ByValue]
     :super Structure
     :flags #{:public}
     :fields (into []
                   (map (fn [field]
                          (let [type (coffi-type->insn-type struct-prefix (:datatype field))]
                            (merge
                             {:flags #{:public}
                              :name (field-name field)
                              :type type}))))
                   fields)
     :methods
     [{:name :init
       :emit (vec
              (concat
               [[:aload 0]
                [:invokespecial :super :init [:void]]]

               (eduction
                (filter (fn [{t :datatype}]
                          (and (vector? t)
                               (= :coffi.mem/array (first t)))))
                (mapcat (fn [{:keys [datatype] :as field}]
                          (let [[_ t size] datatype
                                array-type (coffi-type->insn-type struct-prefix t)]
                            [[:aload 0]
                             [:ldc size]
                             
                             (if (insn-util/array-type-keyword? array-type )
                               [:newarray array-type]
                               [:anewarray array-type])
                             [:putfield :this (field-name field) (coffi-type->insn-type struct-prefix datatype)]
                             ])))
                fields)


               [[:return]]))}
      {:name :valAt
       :desc [Object Object]
       :emit
       [(load-var-inst `structure_valAt)
        [:aload 0]
        [:aload 1]
        [:invokeinterface IFn "invoke" [Object Object Object]]
        [:areturn]]}]
     
     :annotations {com.sun.jna.Structure$FieldOrder
                   (mapv field-name fields)}}))

(defn struct->class-by-ref [struct-prefix struct]
  (assoc (struct->class-def* struct-prefix struct)
         :name (symbol (str struct-prefix "." (name (:id struct)) "ByReference"))
         :interfaces [Structure$ByReference ILookup]))

(defn struct->class-by-value [struct-prefix struct]
  (assoc (struct->class-def* struct-prefix struct)
         :name (symbol (str struct-prefix "." (name (:id struct))))
         :interfaces [Structure$ByValue ILookup]))

(defn def-struct [struct-prefix struct]
  (let [by-ref-def (struct->class-by-ref struct-prefix struct)
        by-value-def (struct->class-by-value struct-prefix struct)]
    (insn/define by-ref-def)
    (insn/define by-value-def)))

(defn import-struct [struct-prefix struct]
  (let [by-ref-def (struct->class-by-ref struct-prefix struct)
        by-value-def (struct->class-by-value struct-prefix struct)]
    (eval `(import ~(:name by-ref-def)))
    (eval `(import ~(:name by-value-def)))))

(defn make-callback-interface* [struct-prefix ret-type arg-types]
  {:flags #{:public :interface}
   :interfaces [Callback] 
   :methods [{:flags #{:public :abstract}
              :name "callback"
              :desc
              (conj (mapv #(coffi-type->insn-type struct-prefix %) arg-types)
                    (coffi-type->insn-type struct-prefix ret-type))}]})

(defn make-callback-interface [struct-prefix ret-type arg-types]
  (insn/define (make-callback-interface* struct-prefix ret-type arg-types)))
(def make-callback-interface-memo (memoize make-callback-interface))

(defn callback-maker* [struct-prefix ret-type arg-types]
  (let [interface (make-callback-interface-memo struct-prefix ret-type arg-types)
        args (map (fn [i] (symbol (str "x" i)))
                  (range (count arg-types)))]
    `(fn [f#]
       (preserve! f#)
       (preserve!
        (reify
          ~(symbol (.getName interface))
          (~'callback [this# ~@args]
           (.setContextClassLoader (Thread/currentThread) main-class-loader)
           (f# ~@args)))))))

(defn callback-maker [struct-prefix ret-type arg-types]
  (eval (callback-maker* struct-prefix ret-type arg-types)))

(defn array? [o]
  (let [c (class o)]
    (.isArray c)))

(defn coercer [struct-prefix t]
  (case t
    :coffi.mem/char byte
    :coffi.mem/short short
    :coffi.mem/int int
    :coffi.mem/long long
    :coffi.mem/float float    
    :coffi.mem/double double
    :coffi.mem/pointer (fn [o]
                         (cond
                           (instance? com.sun.jna.Structure$ByReference o)
                           (.getPointer o)

                           (not (or (nil? o)
                                    (string? o)
                                    (array? o)
                                    (instance? Pointer o)
                                    (instance? PointerType o)))
                           (throw (ex-info "Must be a pointer"
                                             {:o o}))

                           :else o))

    (cond
      (vector? t)
      (case (first t)
        :coffi.mem/pointer (coercer struct-prefix :coffi.mem/pointer)

        :coffi.ffi/fn
        (let [->callback
              (callback-maker struct-prefix (nth t 2) (nth t 1))]
          
          (fn [o]
            (if (instance? Callback o)
              o
              (->callback o))))
        
        :coffi.mem/array
        (fn [o]
          (into-array (class o)
                      o)))
      

      (keyword? t)
      (if (= "coffi.mem" (namespace t))
        (throw (ex-info "Unknown coffi type."
                        {:t t}))
        ;;else
        (let [cls (Class/forName (str struct-prefix "." (name t)))]
          (fn [o]
            (when-not (instance? cls o)
              (throw (ex-info (str "Must be a " struct-prefix "." (name t))
                              {:o o})))
            o))))))

(defn fn-ast [struct-prefix f]
  (let [args (mapv (fn [arg]
                     (let [arg-name (:spelling arg)]
                       (symbol
                        (if (= arg-name "")
                          (str/replace (:type arg)
                                       #" "
                                       "_")
                          arg-name))))
                   (:args f))
        fn-name (symbol (:symbol f))
        lib## (gensym "lib_")
        doc-string (let [doc (:raw-comment f)]
                     (str
                      (-> f :ret :spelling) " " (:name f) "("
                      (str/join ", "
                                (eduction
                                 (map (fn [arg]
                                        (str (:type arg)
                                             " "
                                             (:spelling arg)))
                                      (:args f))))
                      ")"
                      "\n"
                      doc))
        cfn-sym (with-meta (gensym "cfn") {:tag 'com.sun.jna.Function})
        fn-def `(fn [~lib##]
                  (let [struct-prefix# ~struct-prefix
                        ret-type# (coffi-type->jna struct-prefix#
                                                   ~(:function/ret f))
                        coercers#
                        (doall (map #(coercer struct-prefix# %) ~(:function/args f)))

                        ~cfn-sym (.getFunction ~(with-meta lib## {:tag 'com.sun.jna.NativeLibrary})
                                               ~(name fn-name))]
                    (fn ~fn-name ~args
                      (let [args# (map (fn [coerce# arg#]
                                         (coerce# arg#))
                                       coercers#
                                       ~args)]
                        (.invoke ~cfn-sym
                                 ret-type# (to-array args#))))))]
    {:name fn-name
     :doc-string doc-string
     :args args
     :->fn fn-def
     :->defn
     `(fn [~lib##]
        (let [;; delay looking up function symbol
              ;; until needed.
              f# (delay (~fn-def ~lib##))]
          (defn ~fn-name ~doc-string ~args
            (@f# ~@args))))}))

(def POINTER-TYPES
  (specter/recursive-path [] p
	                  (specter/if-path (fn [t]
                                             (when (and (vector? t)
                                                        (>= (count t) 2))
                                               (let [type (first t)]
                                                 (or (= type :coffi.mem/pointer)
                                                     (= type :coffi.mem/array)
                                                     (= type :coffi.ffi/fn)))))
		                           (specter/stay-then-continue
                                            [;; make sure selection is
                                             ;; still a vector after transformation
                                             vector?

                                             (specter/if-path #(= :coffi.ffi/fn (first %))
                                                              (specter/multi-path
                                                               ;; args
                                                               [(specter/nthpath 1) specter/ALL]
                                                               ;; ret
                                                               (specter/nthpath 2))

                                                              ;; else
                                                              (specter/nthpath 1))
                                             ;; recurse
                                             p])
                                           ;; else, not a compound type
                                           specter/STAY)))


;; Where possible, we want to use MyStructByReference
;; to make the generated API easier to use.
;; However, we can't use MyStructByReference
;; if MyStruct is a forward declaration
;; and we don't actually know the size or fields.
;; In those cases, we'll just use Pointer.
(defn replace-forward-references [api]
  (let [known-struct-types (->> api
                                :structs
                                (map :id)
                                (into #{}))
        function-types-path
        [:functions
         specter/ALL
         (specter/multi-path
          :function/ret
          [:function/args specter/ALL])]

        struct-types-path
        [:structs
         specter/ALL
         :fields
         specter/ALL
         :datatype]

        qualified-pointer-type?
        (fn [t]
          (and (vector? t)
               (= :coffi.mem/pointer
                  (first t))
               (keyword (second t))

               (not= "coffi.mem"
                     (namespace (second t)))))
        ref-path [(specter/multi-path function-types-path
                                      struct-types-path)
                  POINTER-TYPES
                  qualified-pointer-type?
                  (fn [[_ pointee-type]]
                    (not (contains? known-struct-types pointee-type)))]]
    #_(specter/select ref-path
                      api)
    (specter/setval ref-path
                    :coffi.mem/pointer
                    api)))



(defn def-enum* [enum]
  `(def ~(-> enum
             :name
             symbol)
     ~@(when-let [doc (:raw-comment enum)]
         (when (not= doc "")
           [doc]))
     (int ~(:value enum))))

(defmacro def-enum [enum]
  (def-enum* enum))

(defmacro import-structs!
  ([api]
   `(import-structs! ~api ~(str (munge (ns-name *ns*))
                                "."
                                "structs")))
  ([api struct-prefix]
   `(let [api# (replace-forward-references ~api)
          struct-prefix# ~struct-prefix]
      (run! #(import-struct struct-prefix# %) (:structs api#)))))

(defn ns-struct-prefix [ns]
  (str (munge (ns-name ns))
       "."
       "structs"))

(defmacro def-enums
  ([enums]
   `(run! #(eval (def-enum* %)) ~enums)))

(defmacro def-structs [structs struct-prefix]
  `(run! #(def-struct ~struct-prefix %) ~structs))

(defmacro def-functions [lib functions struct-prefix]
  `(run! (fn [fdef#]
           (let [def-form# (-> (fn-ast ~struct-prefix fdef#)
                               :->defn)]
             ((eval def-form#) ~lib)))
         ~functions))

(defmacro def-api
  ([lib api]
   `(def-api ~lib ~api ~(ns-struct-prefix *ns*)))
  ([lib api struct-prefix]
   `(let [api# (replace-forward-references ~api)
          lib# ~lib
          struct-prefix# ~struct-prefix]
      (def-enums (:enums api#))
      (def-structs (:structs api#) struct-prefix#)
      (def-functions lib# (:functions api#) struct-prefix#))))

(comment
  (require '[no.disassemble.r :as r]
           '[clojure.java.io :as io])
  (import '(org.eclipse.jdt.internal.core.util
            ClassFileReader)
          '(org.eclipse.jdt.core.util IClassFileReader))
  (defn reader->map [r]
    {:attributes      (->> r .getAttributes (map r/coerce))
     :major-version   (.getMajorVersion r)
     :minor-version   (.getMinorVersion r)
     :class?          (.isClass r)
     :interface?      (.isInterface r)
     :name            (symbol (String. (.getClassName r)))
     :superclass-name (symbol (String. (.getSuperclassName r)))
     :interface-names (.getInterfaceNames r)
     :fields          (->> r .getFieldInfos (map r/coerce))
     :methods         (->> r .getMethodInfos (map r/coerce))})

  (defn file->bytes [fname]
    (let [bos (java.io.ByteArrayOutputStream.)]
      (with-open [fis (java.io.FileInputStream. (io/file fname))]
        (io/copy fis bos))
      (.toByteArray bos)))

  (def class-info
    (reader->map
     (ClassFileReader. (file->bytes "target/classes/com/phronemophobic/clong/MyCallback.class") IClassFileReader/ALL)))

  (clojure.pprint/pprint class-info)

  (defn print-class [class-info]
    (clojure.pprint/pprint
     (reader->map
      (ClassFileReader. (insn/get-bytes class-info) IClassFileReader/ALL)))
    )

  )

(def TYPE-TREE
  "Specter navigator that will visit the current type
  and all the related types of a
  compound type (ie. pointers, arrays, functions, etc)."
  (specter/recursive-path
   [] p
   (specter/if-path
    vector?
    (specter/cond-path
     (fn [t]
       (and (#{:coffi.mem/pointer
               :coffi.mem/array} (first t))
            (second t)))
     (specter/continue-then-stay [(specter/nthpath 1) p])

     (fn [t]
       (and (= :coffi.ffi/fn (first t))
            (>= (count t) 3)))
     (specter/continue-then-stay
      (specter/multi-path
       ;; args
       [(specter/nthpath 1) specter/ALL p]
       ;; ret
       [(specter/nthpath 2) p]))

     ;; else
     (constantly true) specter/STAY)

    ;; else
    specter/STAY)))

(def FUNCTION-TYPES
  "Specter navigator that visits all types of a functoin definition."
  (specter/multi-path
   (specter/keypath :function/ret)
   (specter/path (specter/keypath :function/args) specter/ALL)))
(def STRUCT-TYPES
  "Specter navigator that visits all types of a struct definition."
  [(specter/keypath :fields)
   specter/ALL
   (specter/keypath :datatype)])

(def ALL-TYPES
  "Specter navigator that visits all types of an api."
  (specter/multi-path
   (specter/path
     :functions specter/ALL FUNCTION-TYPES)
   (specter/path
     :structs specter/ALL STRUCT-TYPES)))

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
           java.util.List))

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

(defn struct->class-def* [struct-prefix struct]
  (let [fields (:fields struct)]
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
                              :name (:name field)
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
                (mapcat (fn [{:keys [datatype name]}]
                          (let [[_ t size] datatype
                                array-type (coffi-type->insn-type struct-prefix t)]
                            [[:aload 0]
                             [:ldc size]
                             
                             (if (insn-util/array-type-keyword? array-type )
                               [:newarray array-type]
                               [:anewarray array-type])
                             [:putfield :this name (coffi-type->insn-type struct-prefix datatype)]
                             ])))
                fields)


               [[:return]]))}]
     
     :annotations {com.sun.jna.Structure$FieldOrder
                   (mapv :name fields)}}))

(defn struct->class-by-ref [struct-prefix struct]
  (assoc (struct->class-def* struct-prefix struct)
         :name (symbol (str struct-prefix "." (name (:id struct)) "ByReference"))
         :interfaces [Structure$ByReference]))

(defn struct->class-by-value [struct-prefix struct]
  (assoc (struct->class-def* struct-prefix struct)
         :name (symbol (str struct-prefix "." (name (:id struct))))
         :interfaces [Structure$ByValue]))

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

(defn def-fn* [struct-prefix f]
  (let [args (mapv (fn [arg]
                     (let [arg-name (:spelling arg)]
                       (symbol
                        (if (= arg-name "")
                          (str/replace (:type arg)
                                       #" "
                                       "_")
                          arg-name))))
                   (:args f))
        cfn-sym (with-meta (gensym "cfn") {:tag 'com.sun.jna.Function})
        fn-name (symbol (:symbol f))
        lib## (gensym "lib_")
]
    `(fn [~lib##]
       (let [struct-prefix# ~struct-prefix
             ret-type# (coffi-type->jna struct-prefix#
                                        ~(:function/ret f))
             coercers#
             (doall (map #(coercer struct-prefix# %) ~(:function/args f)))]
         (defn ~fn-name
           ~(let [doc (:raw-comment f)]
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
           ~args
           (let [~cfn-sym (.getFunction ~(with-meta lib## {:tag 'com.sun.jna.NativeLibrary})
                                        ~(name fn-name))
                 args# (map (fn [coerce# arg#]
                              (coerce# arg#))
                            coercers#
                            ~args)]
             #_(prn "invoking "
                  ~(name fn-name)
                  (mapv type args#)
                  args#)
             (.invoke ~cfn-sym
                      ret-type# (to-array args#))))))))

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


(defmacro def-api
  ([lib api]
   `(def-api ~lib ~api ~(str (munge (ns-name *ns*))
                             "."
                             "structs")))
  ([lib api struct-prefix]
   `(let [api# (replace-forward-references ~api)
          lib# ~lib
          struct-prefix# ~struct-prefix]
      (run! #(eval (def-enum* %))
            (:enums api#))
      (run! #(def-struct struct-prefix# %) (:structs api#))
      (run! #((eval (def-fn* struct-prefix# %)) lib#) (:functions api#))
      (run! #(import-struct struct-prefix# %) (:structs api#)))))


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

(ns com.phronemophobic.clong.gen.libffi
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [com.rpl.specter :as specter]
            [clojure.edn :as edn])
  (:import java.io.PushbackReader
           org.bytedeco.libffi.global.ffi
           org.bytedeco.libffi.ffi_cif
           org.bytedeco.libffi.ffi_type
           org.bytedeco.javacpp.Loader
           org.bytedeco.javacpp.Pointer
           org.bytedeco.javacpp.BytePointer
           org.bytedeco.javacpp.CharPointer
           org.bytedeco.javacpp.DoublePointer
           org.bytedeco.javacpp.FloatPointer
           org.bytedeco.javacpp.IntPointer
           org.bytedeco.javacpp.LongPointer
           org.bytedeco.javacpp.PointerPointer
           org.bytedeco.javacpp.ShortPointer
           
           )
  )

;; The main problem with javacpp's libffi
;; is that it doesn't seem to support creating
;; callbacks without Java centric generation.
;; http://bytedeco.org/javacpp/apidocs/org/bytedeco/javacpp/FunctionPointer.html

#_(Loader/addressof "objc_msgSend")

(defn coffi-type->ffi [t]
  (case t
    :coffi.mem/char (ffi/ffi_type_sint8)
    :coffi.mem/short (ffi/ffi_type_sint16)
    :coffi.mem/int (ffi/ffi_type_sint32)
    :coffi.mem/long (ffi/ffi_type_sint64)
    :coffi.mem/float (ffi/ffi_type_float) 
    :coffi.mem/double (ffi/ffi_type_double)
    :coffi.mem/pointer (ffi/ffi_type_pointer)
    :coffi.mem/void (ffi/ffi_type_void)
    
    (cond
      (vector? t)
      (case (first t)
        :coffi.mem/pointer (ffi/ffi_type_pointer)
        

        :coffi.ffi/fn (ffi/ffi_type_pointer)
        ;;com.sun.jna.Callback

        :coffi.mem/struct
        (let [fields (second t)
              elements (PointerPointer.
                        ;; null terminated
                        (inc (count fields)))
              ftype (doto (ffi_type.)
                      (.alignment 0)
                      (.size 0)
                      (.type (ffi/FFI_TYPE_STRUCT))
                      (.elements elements))]
          (doseq [[i [_field-name datatype]] (map-indexed vector fields)]
            (.put elements i (coffi-type->ffi datatype)))
          ;; null terminated
          (.put elements (count fields) nil))
        
        
        :coffi.mem/array
        (let [[_ array-type array-length] t]
          ;; suggested workaround https://www.chiark.greenend.org.uk/doc/libffi-dev/html/Arrays-Unions-Enums.html
          ;; To emulate an array, simply create an ffi_type using FFI_TYPE_STRUCT with as many members as there are elements in the array.
          (coffi-type->ffi
           [:coffi.mem/struct
            (into []
                  (map (fn [i]
                         [(keyword (str "x" i)) array-type]))
                  (range array-length))])))

      (keyword? t)
      (throw (ex-info "Unknown coffi type."
                        {:t t})))))






;; {
;;   ffi_type tm_type;
;;   ffi_type *tm_type_elements[12];
;;   int i;

;;   tm_type.size = tm_type.alignment = 0;
;;   tm_type.type = FFI_TYPE_STRUCT;
;;   tm_type.elements = &tm_type_elements;
    
;;   for (i = 0; i < 9; i++)
;;       tm_type_elements[i] = &ffi_type_sint;

;;   tm_type_elements[9] = &ffi_type_slong;
;;   tm_type_elements[10] = &ffi_type_pointer;
;;   tm_type_elements[11] = NULL;

;;   /* tm_type can now be used to represent tm argument types and
;;      return types for ffi_prep_cif() */
;; }


(defn coercer [t]
  (case t
    :coffi.mem/char #(doto (BytePointer. 1)
                       (.put (byte %)))
    :coffi.mem/short #(doto (ShortPointer. 1)
                        (.put (short %)))
    :coffi.mem/int #(doto (IntPointer. 1)
                      (.put (int %)))
    :coffi.mem/long #(doto (LongPointer. 1)
                       (.put (long %)))
    :coffi.mem/float #(doto (FloatPointer. 1)
                        (.put (float %)))
    :coffi.mem/double #(doto (DoublePointer. 1)
                         (.put (double %)))
    :coffi.mem/pointer (fn [^Pointer p]
                         (doto (PointerPointer. 1)
                           (.put p)))

    (cond
      (vector? t)
      (case (first t)
        :coffi.mem/pointer (recur :coffi.mem/pointer)

        :coffi.ffi/fn
        ;; !! javacpp doesn't seem to support callbacks created at runtime
        ;; it seems like javacpp requires callbacks be generated
        ;; http://bytedeco.org/javacpp/apidocs/org/bytedeco/javacpp/FunctionPointer.html
        (throw (ex-info "callbacks not supported yet"
                        {:t t}))
        #_(let [->callback
                (callback-maker struct-prefix (nth t 2) (nth t 1))]
          
            (fn [o]
              (if (instance? Callback o)
                o
                (->callback o))))
        
        :coffi.mem/array
        (throw (ex-info "arrays not supported yet"
                        {:t t}))
        #_(fn [o]
            ()))
      

      (keyword? t)
      (if (= "coffi.mem" (namespace t))
        (throw (ex-info "Unknown coffi type."
                        {:t t}))
        ;;else
        (throw (ex-info "structs not supported yet."
                        {:t t}))
        ))))

(defn uncoercer [t]
  (case t
    :coffi.mem/char (fn [^BytePointer p]
                      (.get p))
    :coffi.mem/short (fn [^ShortPointer p]
                       (.get p))
    :coffi.mem/int (fn [^IntPointer p]
                     (.get p))
    :coffi.mem/long (fn [^LongPointer p]
                      (.get p))
    :coffi.mem/float (fn [^FloatPointer p]
                       (.get p))
    :coffi.mem/double (fn [^DoublePointer p]
                        (.get p))
    :coffi.mem/pointer (fn [^PointerPointer p]
                         (.get p))

    (cond
      (vector? t)
      (case (first t)
        :coffi.mem/pointer (recur :coffi.mem/pointer)

        :coffi.ffi/fn
        (throw (ex-info "callbacks not supported yet"))
        #_(let [->callback
                (callback-maker struct-prefix (nth t 2) (nth t 1))]
          
            (fn [o]
              (if (instance? Callback o)
                o
                (->callback o))))
        
        :coffi.mem/array
        (throw (ex-info "arrays not supported yet"))
        #_(fn [o]
            ()))
      

      (keyword? t)
      (if (= "coffi.mem" (namespace t))
        (throw (ex-info "Unknown coffi type."
                        {:t t}))
        ;;else
        (throw (ex-info "structs not supported yet."
                        {:t t}))
        ))))

(defn return-pointer [t]
  (case t

    ;; In most situations, ‘libffi’ will handle promotion according to the ABI. However, for historical reasons, there is a special case with return values that must be handled by your code. In particular, for integral (not struct) types that are narrower than the system register size, the return value will be widened by ‘libffi’. ‘libffi’ provides a type, ffi_arg, that can be used as the return type. For example, if the CIF was defined with a return type of char, ‘libffi’ will try to store a full ffi_arg into the return value.

    ;; tldr, shorts, bytes, and ints need have enough space to hold longs
    :coffi.mem/char (BytePointer. 8)
    :coffi.mem/short (ShortPointer. 4)
    :coffi.mem/int (IntPointer. 2)

    
    :coffi.mem/long (LongPointer. 1)
    
    :coffi.mem/float (FloatPointer. 1)
    
    :coffi.mem/double (DoublePointer. 1)
    
    :coffi.mem/pointer (PointerPointer. 1)
    

    (cond
      (vector? t)
      (case (first t)
        :coffi.mem/pointer (recur :coffi.mem/pointer)

        :coffi.ffi/fn
        (throw (ex-info "callbacks not supported yet"))
        #_(let [->callback
                (callback-maker struct-prefix (nth t 2) (nth t 1))]
          
            (fn [o]
              (if (instance? Callback o)
                o
                (->callback o))))
        
        :coffi.mem/array
        (throw (ex-info "arrays not supported yet"))
        #_(fn [o]
            ()))
      

      (keyword? t)
      (if (= "coffi.mem" (namespace t))
        (throw (ex-info "Unknown coffi type."
                        {:t t}))
        ;;else
        (throw (ex-info "structs not supported yet."
                        {:t t}))
        ))))

(defn make-cif [ret args]
  (let [cif (ffi_cif.)
        num-args (count args)

        
        ^ffi_type ret-type (coffi-type->ffi ret)

        
        arg-types (PointerPointer. num-args)
        _ (doseq [[i arg] (map-indexed vector args)]
            (.put arg-types i (coffi-type->ffi arg)))

        status (ffi/ffi_prep_cif cif
                                 (ffi/FFI_DEFAULT_ABI)
                                 num-args
                                 ret-type
                                 arg-types)]
    (when (not= status ffi/FFI_OK)
      (throw (ex-info "Invalid c function interface."
                      {:ret ret
                       :args args
                       :status status})))
    cif))

(defn fn-ast [f]
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
        fn-address## (gensym "fn-address-")
        cif## (gensym "cif-")
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

        ]
    {:name fn-name
     :doc-string doc-string
     :args args
     ;; :->fn fn-def
     :->defn
     `(let [ ;; delay looking up function symbol
            ;; until needed.
            
            ~fn-address## (delay
                            (Loader/addressof ~(name fn-name)))
            ~cif## (delay (make-cif ~(:function/ret f)
                                    ~(:function/args f)))
            args# (PointerPointer. ~(count (:function/args f)))
            ^Pointer
            returnp# (return-pointer ~(:function/ret f))

            coercers# (into []
                            (map (fn [t#]
                                   (coercer t#)))
                            ~(:function/args f))
            uncoercer# (uncoercer ~(:function/ret f))]
        (defn ~fn-name ~doc-string ~args
          (doseq [[i# coercer# arg#] (map vector (range) coercers# ~args)]
            (let [p# (coercer# arg#)]
              (.put args# i# p#)))
          (let [^ffi_cif cif# @~cif##
                ^Pointer fn-address# @~fn-address##]
            (ffi/ffi_call cif# fn-address# returnp# args#
                          ))
          (uncoercer# returnp#)))}))

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

(defn struct->coffi-type [structs-by-id struct]
  (println (:id struct))
  [:coffi.mem/struct
   (into []
         (map (fn [field]
                [(keyword (:name field))

                 (let [datatype (:datatype field)]
                   (if-let [field-struct (get structs-by-id datatype)]
                     (struct->coffi-type structs-by-id field-struct)
                     datatype))]))
         (:fields struct))])

(defn concretize-structs [api]
  (let [structs (:structs api)
        structs-by-id (into {}
                            (map (juxt :id identity))
                            structs)
        struct-type? (fn [datatype]
                       (contains? structs-by-id datatype))]
    (specter/transform [:functions specter/ALL FUNCTION-TYPES struct-type?]
                       #(struct->coffi-type structs-by-id (get structs-by-id %))
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

(defmacro def-enums
  ([enums]
   `(run! #(eval (def-enum* %)) ~enums)))

(defmacro def-functions [functions]
  `(run! (fn [fdef#]
           (let [def-form# (-> (fn-ast fdef#)
                               :->defn)]
             (eval def-form#)))
         ~functions))

(defmacro def-api
  ([api]
   `(let [api# (-> ~api
                   replace-forward-references
                   concretize-structs)]
      (def-enums (:enums api#))
      #_(def-structs (:structs api#) struct-prefix#)
      (def-functions (:functions api#)))))

(comment

  (def graphviz-api (with-open [rdr (io/reader
                                     (io/file
                                      "../../../clj-graphviz/resources/com/phronemophobic/clj-graphviz/api.edn"))
                                rdr (java.io.PushbackReader. rdr)]
                      (edn/read rdr)))

  (concretize-structs graphviz-api)
  ,)


(comment
 (def dest (BytePointer. 255))
 (def dest-size (doto (LongPointer. 1)
                  (.put (.capacity dest))))

 (def source (BytePointer. "clong!"))

 (->> (:functions clong.libz/api)
      (filter #(= "uncompress"
                  (:symbol %)))
      first
      fn-ast
      :->defn
      eval)

 (->> (:functions clong.libz/api)
      (filter #(= "compress"
                  (:symbol %)))
      first
      fn-ast
      :->defn
      eval)

 (->> (:functions clong.libz/api)
      (filter #(= "uncompress"
                  (:symbol %)))
      first
      fn-ast
      :->defn
      clojure.pprint/pprint)



 (def result (compress dest dest-size source (dec (.capacity source))))
 ;; 0 

 (.get dest-size) ;; 15


 ;; (def dest (byte-array 255))
 ;; (def dest-size* (doto (LongByReference.)
 ;;                   (.setValue (alength dest))))

 ;; (compress  dest dest-size* source (count source)) ;; 0

 ;; (.getValue dest-size*) ;; 14
 (def dest2 (BytePointer. (dec (.capacity source))))
 (def dest2-size (doto (LongPointer. 1)
                   (.put (.capacity dest2))))

 (uncompress dest2 dest2-size dest (.get dest-size)) ;; 0

 (.getString dest2)


 ;; (def dest2 (byte-array (count source)))
 ;; (def dest2-size* (doto (LongByReference.)
 ;;                    (.setValue (alength dest2))))
 ;; (uncompress dest2 dest2-size* dest (.getValue dest-size*)) ;; 0

 ;; (String. dest2) ;; "clong!"
 ,)

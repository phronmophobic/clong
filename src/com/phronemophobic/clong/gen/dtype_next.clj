(ns com.phronemophobic.clong.gen.dtype-next
  (:require [clojure.string :as str]
            [tech.v3.datatype.ffi :as dt-ffi]))

;; def struct
;; - structs with callback fields (this is used heavily by [cef](https://github.com/martyr-deepin/cef-binary/blob/fecf00339545d2819224333cc506d5aa22ae8008/include/capi/cef_client_capi.h#L77)). Can be done with pointers?
;; - structs with struct-by-reference fields. I know you could just use a pointer, but it seems like accessing substructs is awkward.

;; Nice to haves:
;; - bitfields (not supported by libffi and by extension, [jna](https://github.com/java-native-access/jna/issues/423))
;; - unions

(defn coffi-type->dtype [t]
  (case t
    :coffi.mem/char :int8
    :coffi.mem/short :int16
    :coffi.mem/int :int32
    :coffi.mem/long :int64
    :coffi.mem/float :float32
    :coffi.mem/double :float64
    :coffi.mem/pointer :pointer?
    :coffi.mem/void :void
    
    (cond
      (vector? t)
      (case (first t)
        :coffi.mem/pointer :pointer?

        :coffi.ffi/fn :pointer?
        ;;com.sun.jna.Callback
        
        :coffi.mem/array
        (throw (ex-info "Unsupported coffi type."
                        {:t t})))

      (keyword? t)
      (if (= "coffi.mem" (namespace t))
        (throw (ex-info "Unknown coffi type."
                        {:t t}))
        ;;else
        (throw (ex-info "Unsupported coffi type."
                        {:t t}))))))

(defn clong-fn->dt-type-fn [fdef]
  (let [kw (:id fdef)
        rettype (coffi-type->dtype (:function/ret fdef))
        argtypes (into []
                       (map-indexed
                        (fn [i [{:keys [spelling]} t]]
                          (let [argname
                                (if (seq spelling)
                                  spelling
                                  (str "__unnamed_arg_" i))]
                            [(symbol argname)
                             (coffi-type->dtype t)])))
                       (map vector
                            (:args fdef)
                            (:function/args fdef)))
        doc-string (let [doc (:raw-comment fdef)]
                     (str
                      (-> fdef :ret :spelling) " " (:name fdef) "("
                      (str/join ", "
                                (eduction
                                 (map (fn [arg]
                                        (str (:type arg)
                                             " "
                                             (:spelling arg)))
                                      (:args fdef))))
                      ")"
                      "\n"
                      doc))]
    {kw {:rettype rettype
         :argtypes argtypes
         :doc doc-string}}))

(defn clong-struct->dt-struct [s]
  (let [id (-> s :id name keyword)
        fields (into []
                     (map (fn [field]
                            (let [dtype (-> field
                                            :datatype
                                            coffi-type->dtype)
                                  dtype (if (= dtype :pointer?)
                                          ;; :pointer? is invalid for structs
                                          :pointer
                                          dtype)]
                             {:name (-> field :name keyword)
                              :datatype dtype})))
                     (:fields s))]
    [id fields]))

(defn api->structs [api]
  (into []
        (map clong-struct->dt-struct)
        (:structs api)))

(defn api->library-interface [api]
  (into {}
        (map clong-fn->dt-type-fn)
        (:functions api)))


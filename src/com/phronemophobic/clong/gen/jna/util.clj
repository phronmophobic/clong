(ns com.phronemophobic.clong.gen.jna.util
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [insn.util :as insn-util]
            [insn.core :as insn]
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
           com.sun.jna.NativeMapped
           clojure.java.api.Clojure
           clojure.lang.IFn
           clojure.lang.ILookup
           clojure.lang.Seqable
           clojure.lang.ISeq))




(definterface IGetFieldOrder
  (^java.util.List getFieldOrder []))

;; var needs to exist.
;; referenced by var in Structure class definitions
(defn ^:dynamic structure_valAt
  ([s k]
   (.readField ^Structure s (name k))))

;; var needs to exist.
;; referenced by var in Structure class definitions
(defn ^:dynamic structure_seq
  ([^IGetFieldOrder s]
   (let [fields (.getFieldOrder s)]
     (map (fn [field-name]
            (reify
              java.util.Map$Entry
              (getKey [_]
                (keyword field-name))
              (getValue [_]
                (get s field-name))))
          fields))))

(defn ^:private type-desc [struct-prefix t]
  (case t
    :coffi.mem/char "B"
    :coffi.mem/short "S"
    :coffi.mem/int "I"
    :coffi.mem/long "J"
    :coffi.mem/float "F"
    :coffi.mem/double "D"
    :coffi.mem/pointer "Lcom/sun/jna/Pointer;"
    :coffi.mem/void "V"

    (cond
      (vector? t)
      (case (first t)
        :coffi.mem/pointer
        (let [ptype (second t)]
          (case ptype
            :coffi.mem/char "Lcom/sun/jna/ptr/ByteByReference;"
            :coffi.mem/short  "Lcom/sun/jna/ptr/ShortByReference;"
            :coffi.mem/int "Lcom/sun/jna/ptr/IntByReference;"
            :coffi.mem/long "Lcom/sun/jna/ptr/LongByReference;"
            :coffi.mem/float "Lcom/sun/jna/ptr/FloatByReference;"
            :coffi.mem/double "Lcom/sun/jna/ptr/DoubleByReference;"
            :coffi.mem/pointer "Lcom/sun/jna/ptr/PointerByReference;"

            ;; else
            (if-not (keyword ptype)
              "Lcom/sun/jna/Pointer;"
              (cond
                (= ptype :coffi.mem/char)
                "Ljava/lang/String;"

                (not= "coffi.mem"
                      (namespace ptype))
                (str "L" (str/replace struct-prefix #"\." "/") "/" (name ptype) "ByReference;")

                :else "Lcom/sun/jna/Pointer;"))))


        :coffi.ffi/fn "Lcom/sun/jna/Pointer;"
        ;; Callback is abstract. Need specific type info to be useful
        ;;com.sun.jna.Callback
        
        :coffi.mem/array
        (str "[" (type-desc struct-prefix (second t))))
      
      (class? t)
      (str "L" (str/replace (.getName ^Class t) #"\." "/")  ";")

      (keyword? t)
      (if (= "coffi.mem" (namespace t))
        (throw (ex-info "Unknown coffi type."
                        {:t t}))
        ;;else
        (str "L" (str/replace struct-prefix #"\." "/") "/" (name t) ";")))))


(defn callback-name [struct-prefix ret-type arg-types]
  (str/replace
   (munge
    (str
     "callback_"
     (str/join
      "_"
      (eduction
       (map #(type-desc struct-prefix %))
       (into [ret-type] arg-types)))))
   #"[;]"
   "_SEMI_COLON_"))


(defn array-type-desc [struct-prefix t]
  (str "[" (type-desc struct-prefix t)))

(ns com.phronemophobic.clong.gen.jna.insn
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
      
      (class? t)
      t

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





(defn load-var-inst [sym]
  [[:ldc (namespace sym)]
   [:ldc (name sym)]
   [:invokestatic Clojure "var" [Object Object IFn]]])


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
     :super 'com.sun.jna.Structure ;;Structure
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
      {:name :getFieldOrder
       :flags #{:public}
       :desc [List]
       :emit
       (concat
        [[:new java.util.ArrayList]
         [:dup]
         [:invokespecial java.util.ArrayList :init [:void]]
         [:astore 1]]
        (mapcat (fn [field]
                  [[:aload 1]
                   [:ldc (field-name field)]
                   [:invokevirtual java.util.ArrayList "add" [Object :boolean]]
                   [:pop]])
                fields)
        [[:aload 1]
         [:areturn]])}
      {:name :valAt
       :desc [Object Object]
       :emit
       [(load-var-inst `structure_valAt)
        [:aload 0]
        [:aload 1]
        [:invokeinterface IFn "invoke" [Object Object Object]]
        [:areturn]]}
      {:name :seq
       :desc [ISeq]
       :emit
       [(load-var-inst `structure_seq)
        [:aload 0]
        [:invokeinterface IFn "invoke" [Object Object]]
        [:areturn]]}]
     
     :annotations {com.sun.jna.Structure$FieldOrder
                   (mapv field-name fields)}}))



(defn struct->class-by-ref [struct-prefix struct]
  (assoc (struct->class-def* struct-prefix struct)
         :name (symbol (str struct-prefix "." (name (:id struct)) "ByReference"))
         :interfaces [Structure$ByReference ILookup Seqable]))

(defn struct->class-by-value [struct-prefix struct]
  (assoc (struct->class-def* struct-prefix struct)
         :name (symbol (str struct-prefix "." (name (:id struct))))
         :interfaces [Structure$ByValue ILookup Seqable]))

(defn def-struct [struct-prefix struct]
  (let [by-ref-def (struct->class-by-ref struct-prefix struct)
        by-value-def (struct->class-by-value struct-prefix struct)

        by-ref-info (insn/visit by-ref-def)
        by-value-info (insn/visit by-value-def)]
    (when *compile-files*
      (insn/write by-ref-info)
      (insn/write by-value-info))
    (insn/define by-ref-def)
    (insn/define by-value-def)))

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
        (str "[" (type-desc (second t))))
      
      (class? t)
      (str "L" (str/replace (.getName t) #"\." "/")  ";")

      (keyword? t)
      (if (= "coffi.mem" (namespace t))
        (throw (ex-info "Unknown coffi type."
                        {:t t}))
        ;;else
        (str "L" (str/replace struct-prefix #"\." "/") "/" (name t) ";")))))

(defn ^:private callback-name [struct-prefix ret-type arg-types]
  (munge
   (str
    "callback_"
    (str/join
     "_"
     (eduction 
      (map #(type-desc struct-prefix %))
      (into [ret-type] arg-types))))))

(defn make-callback-interface* [struct-prefix ret-type arg-types]
  {:flags #{:public :interface}
   :interfaces [Callback]
   :name (symbol (str struct-prefix "." (callback-name struct-prefix ret-type arg-types)))
   :methods [{:flags #{:public :abstract}
              :name "callback"
              :desc
              (conj (mapv #(coffi-type->insn-type struct-prefix %) arg-types)
                    (coffi-type->insn-type struct-prefix ret-type))}]})

(defn make-callback-interface [struct-prefix ret-type arg-types]
  (when *compile-files*
    (insn/write (make-callback-interface* struct-prefix ret-type arg-types)))
  (insn/define (make-callback-interface* struct-prefix ret-type arg-types)))


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

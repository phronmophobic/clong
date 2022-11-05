(ns com.phronemophobic.clong.clang.coffi.raw
  (:require [coffi.mem :as mem :refer [defalias]]
            [coffi.ffi :as ffi :refer [defcfn]]
            [clojure.string :as str]
            [coffi.layout :as layout]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import java.io.PushbackReader))

(def clang-api (with-open [rdr (io/reader (io/file "resources"
                                                   "clang-api.edn"))
                           pbr (PushbackReader. rdr)]
                 (edn/read pbr)))

(defonce load-clang
  (ffi/load-system-library "clang"))

(defn clang-layout [struct]
  (loop [offset 0
         fields (seq (:fields struct))
         layout []]
    (if fields
      (let [field (first fields)
            calculated-offset (/ (:calculated-offset field)
                                 8)
            offset-diff (- calculated-offset
                           offset)
            _ (assert (not (neg? offset-diff)))
            layout (if (pos? offset-diff)
                     (conj layout
                           [::layout/padding [::mem/padding offset-diff]])
                     layout)
            datatype (:datatype field)]
        (recur
         (+ (mem/size-of datatype) calculated-offset)
         (next fields)
         (conj layout [(keyword (:name field)) datatype])))
      (let [layout-size (mem/size-of [::mem/struct layout])
            final-padding (- (:size-in-bytes struct)
                             layout-size)]
        (if (pos? final-padding)
          (conj layout [::layout/padding [::mem/padding final-padding]])
          layout)))))

(defn def-struct* [struct]
  `(defalias ~(:id struct)
    [::mem/struct
     (clang-layout (quote ~struct))]))

(defmacro def-struct [struct]
  (def-struct* struct))

(defmacro def-structs [structs]
  `(do
     ~@(for [struct (eval structs)]
         `(def-struct ~struct))))

(def-structs (:structs clang-api))

(comment
  (doseq [{:keys [id size-in-bytes] :as struct} (:structs clang-api)
          :when (not= size-in-bytes
                      (mem/size-of id))]
    (prn id size-in-bytes (mem/size-of id)
         (map :datatype (:fields struct))))
  ,)




(def structs-by-id
  (into {}
        (map (juxt :id identity))
        (:structs clang-api)))



#_(defcfn strlen
  "Given a string, measures its length in bytes."
  strlen [::mem/c-string] ::mem/long)

(defn simplify-pointer [t]
  (cond
    (= t [:coffi.mem/pointer :coffi.mem/char]) ::mem/c-string

    (and (vector? t)
         (= ::mem/pointer (first t))
         #_#_(keyword? (second t))
         (not= "coffi.mem"
               (namespace (second t))))
    ::mem/pointer

    :else t))

(defn def-fn* [f]
  `(do
     (defcfn ~(symbol (:symbol f))
       ~(let [doc (:doc f)]
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
       {:arglists
        (list
         ~(mapv (fn [arg]
                  (let [arg-name (:spelling arg)]
                    (list
                     'quote
                     (symbol
                      (if (= arg-name "")
                        (str/replace (:type arg)
                                     #" "
                                     "_")
                        arg-name)))))
                (:args f)))}
       ~(:symbol f)
       ~(mapv simplify-pointer (:function/args f))
       ~(simplify-pointer (:function/ret f)))))

(defmacro def-fn [f]
  (def-fn* f))

(defmacro def-fns [fs]
  `(do
     ~@(for [f (eval fs)]
         `(def-fn ~f))))

(def-fns (:functions clang-api))


(def fns-by-id
  (into {}
        (map (juxt :id identity))
        (:functions clang-api)))


(defn def-enum* [enum]
  `(def ~(-> enum
             :name
             symbol)
     ~@(when-let [doc (:doc enum)]
         (when (not= doc "")
           [doc]))
     ~(:value enum)))

(defmacro def-enum [enum]
  (def-enum* enum))
(defmacro def-enums [enums]
  `(do
     ~@(for [enum (eval enums)]
         `(def-enum ~enum))))

(def-enums (:enums clang-api))

;; (defonce index (clang_createIndex 0 0))
;; (def null-pointer
;;   (doto (mem/alloc-instance ::mem/pointer)
;;     (mem/write-long 0)))
;; (defonce translation-unit (clang_parseTranslationUnit index "/Users/adrian/workspace/clong/csource/align.c" null-pointer 0 null-pointer 0 0) )
;; (def cursor (clang_getTranslationUnitCursor translation-unit))
;; (defn get-children [cursor]
;;   (let [childs (volatile! [])
;;         visitor (fn [child parent _]
;;                   (vswap! childs conj child)
;;                   1)]
;;     (clang_visitChildren cursor
;;                          visitor
;;                          null-pointer)
;;     @childs))y

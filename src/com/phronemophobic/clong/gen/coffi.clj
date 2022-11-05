(ns com.phronemophobic.clong.gen.coffi
  (:require [coffi.mem :as mem :refer [defalias]]
            [coffi.ffi :as ffi :refer [defcfn]]
            [clojure.string :as str]
            [coffi.layout :as layout]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))


(comment
  (import 'java.io.PushbackReader)
  (def clang-api (with-open [rdr (io/reader (io/resource "resources"
                                                         "clang-api.edn"))
                             pbr (PushbackReader. rdr)]
                   (edn/read pbr))))

#_(defn clang-layout [struct]
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
     (layout/with-c-layout
       [::mem/struct
        ~(into []
               (map (fn [field]
                      [(keyword (:name field))
                       (:datatype field)]))
               (:fields struct))])))

(defmacro def-struct [struct]
  (def-struct* struct))

(defmacro def-structs [structs]
  `(do
     ~@(for [struct (eval structs)]
         `(def-struct ~struct))))

;; (def-structs (:structs clang-api))

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
  `(defcfn ~(symbol (:symbol f))
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
     ~(simplify-pointer (:function/ret f))))

(defmacro def-fn [f]
  (def-fn* f))

(defmacro def-fns [fs]
  `(do
     ~@(for [f (eval fs)]
         `(def-fn ~f))))

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
(defmacro def-enums [enums]
  `(do
     ~@(for [enum (eval enums)]
         `(def-enum ~enum))))

;; (def-enums (:enums clang-api))
(defmacro def-api [api]
  `(let [api# ~api]
     (run! #(eval (def-enum* %))
           (:enums api#))
     (run! #(eval (def-struct* %)) (:structs api#))
     (run! #(eval (def-fn* %)) (:functions api#))))

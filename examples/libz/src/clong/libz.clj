(ns clong.libz
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [clong.libz.gen-api :as api]
            [com.phronemophobic.clong.gen.jna :as gen])
  (:import
   java.io.PushbackReader
   com.sun.jna.Memory
   com.sun.jna.Pointer
   com.sun.jna.ptr.PointerByReference
   com.sun.jna.ptr.LongByReference
   com.sun.jna.Structure)
  (:gen-class))

(def libz
  (delay
    (println "loaded!")
    (com.sun.jna.NativeLibrary/getInstance "z")))

(def api (api/load-api))

(gen/def-api-lazy libz api)

(defn -main [& args]
  (let [_ (prn (zlibVersion)) ;; "1.2.11"

        source "clong!"

        dest (byte-array 255)
        dest-size* (doto (LongByReference.)
                     (.setValue (alength dest)))

        _ (prn (compress  dest dest-size* source (count source))) ;; 0

        _ (prn (.getValue dest-size*)) ;; 14

        dest2 (byte-array (count source))
        dest2-size* (doto (LongByReference.)
                      (.setValue (alength dest2)))
        _ (prn (uncompress dest2 dest2-size* dest (.getValue dest-size*))) ;; 0

        _ (prn (String. dest2)) ;; "clong!"
        ]))

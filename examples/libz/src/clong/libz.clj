(ns clong.libz
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [com.phronemophobic.clong.clang :as clong]
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
  (com.sun.jna.NativeLibrary/getInstance "z"))

(def api (clong/easy-api (.getCanonicalPath (io/file "resources"
                                                     "zlib.h"))))

(gen/def-api libz api)

(zlibVersion) ;; "1.2.11"

(def source "clong!")

(def dest (byte-array 255))
(def dest-size* (doto (LongByReference.)
                  (.setValue (alength dest))))

(compress  dest dest-size* source (count source)) ;; 0

(.getValue dest-size*) ;; 14

(def dest2 (byte-array (count source)))
(def dest2-size* (doto (LongByReference.)
                   (.setValue (alength dest2))))
(uncompress dest2 dest2-size* dest (.getValue dest-size*)) ;; 0

(String. dest2) ;; "clong!"


(defn -main [& args])

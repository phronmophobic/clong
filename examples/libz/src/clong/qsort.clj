(ns clong.qsort
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.phronemophobic.clong.gen.jna :as gen])
  (:import com.sun.jna.Memory)
  (:gen-class))


(def capi
  {:functions [{:args
                [{:spelling "__base", :type "void *"}
                 {:spelling "__nel", :type "unsigned long"}
                 {:spelling "__width", :type "unsigned long"}
                 {:spelling "__compar",
                  :type "int (*)(const void *, const void *)"}],
                :ret {:spelling "void"},
                :function/args
                [:coffi.mem/pointer
                 :coffi.mem/long
                 :coffi.mem/long
                 [:coffi.ffi/fn
                  [:coffi.mem/pointer :coffi.mem/pointer]
                  :coffi.mem/int]],
                :symbol "qsort",
                :function/ret :coffi.mem/void,
                :type "CXType_FunctionProto",
                :linkage "CXLinkage_External",
                :id :qsort,
                :raw-comment nil,
                :kind "CXCursor_FunctionDecl",
                :spelling "qsort"}]})

#_(def capi
  ((requiring-resolve 'com.phronemophobic.clong.clang/easy-api)
   (.getCanonicalPath (io/file "/Library/Developer/CommandLineTools/SDKs/MacOSX14.0.sdk/usr/include/stdlib.h"))))

(gen/def-api-lazy (delay (com.sun.jna.NativeLibrary/getProcess)) capi )


(defn mysort [ a  b]
  (let [a (.getByte a 0)
        b (.getByte b 0)]
    (cond
      (= a b) 0
      (> a b) 1
      :else -1)))

(defn -main [& args]
  (let [nums (Memory. 10)
        _ (.write nums 0 (byte-array [1 4 6  9 2 3 93 5 6 3]) 0 10)]
    (qsort nums 10 1
           mysort)
    (prn (seq (.getByteArray nums 0 10)))))

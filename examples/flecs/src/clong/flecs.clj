(ns clong.flecs
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [com.phronemophobic.clong.clang :as clang]))

(comment

  ;; download header file
  (with-open [is (io/input-stream (io/as-url "https://raw.githubusercontent.com/SanderMertens/flecs/v4/flecs.h"))
              os (io/output-stream (io/file "flecs.h"))
              ]
    (io/copy is os))

  (def api
    (clang/easy-api
     (.getCanonicalPath (io/file "flecs.h"))))


  (def documented-functions
    (->> api
         :functions
         (filter :raw-comment)))

  (def documented-enums
    (->> api
         :enums
         (filter :raw-comment))
    )

  ,)

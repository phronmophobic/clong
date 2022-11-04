(ns com.phronemophobic.clong.clang.jna.raw
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [com.phronemophobic.clong.gen.jna :as gen]
            [insn.core :as insn]
            [clojure.pprint :refer [pprint]]
            [insn.util :as insn-util]
            [clojure.edn :as edn])
  (:import java.io.PushbackReader
           com.sun.jna.Memory
           com.sun.jna.Pointer
           com.sun.jna.Platform
           com.sun.jna.ptr.FloatByReference
           com.sun.jna.ptr.IntByReference
           com.sun.jna.IntegerType
           com.sun.jna.Structure$ByValue
           com.sun.jna.Structure
           com.sun.jna.Structure$FieldOrder
           com.sun.jna.Callback
           java.util.List))

(def ^:no-doc libclang
  (com.sun.jna.NativeLibrary/getInstance "clang"))


(def clang-api (with-open [rdr (io/reader (io/resource "clang-api.edn"))
                           pbr (PushbackReader. rdr)]
                 (edn/read pbr)))

(def structs-by-id
  (into {}
        (map (juxt :id identity))
        (:structs clang-api)))

(def fns-by-id
  (into {}
        (map (juxt :id identity))
        (:functions clang-api)))

(gen/def-api libclang "com.phronemophobic.clong.clang.jna.struct" clang-api)



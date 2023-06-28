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

;; setenv("LIBCLANG_DISABLE_CRASH_RECOVERY", "1", 1)
;; https://github.com/dotnet/ClangSharp/issues/167
;; https://reviews.llvm.org/D23662
(def ^:no-doc libc
  (delay (com.sun.jna.NativeLibrary/getInstance "c")))

(let [setenv* (.getFunction @libc "setenv")]
  (defn- setenv [name value overwrite]
    (.invoke setenv* Void/TYPE
             (to-array [name value overwrite]))))
(setenv "LIBCLANG_DISABLE_CRASH_RECOVERY" "1" 0)

(import 'org.bytedeco.llvm.global.clang)
;; This causes libclang to be loaded into the process.
;; Any function would do, but this is a particularly easy example.
(clang/clang_getNullCursor)

(import 'org.bytedeco.javacpp.Loader)
(require 'clojure.pprint)
(clojure.pprint/pprint (Loader/getLoadedLibraries))
(doseq [[k v] (Loader/getLoadedLibraries)]
  (println k ":" v))

(def ^:no-doc libclang
  (com.sun.jna.NativeLibrary/getInstance
   (-> (Loader/getLoadedLibraries)
      (get "clang@.16")))
  #_(com.sun.jna.NativeLibrary/getProcess)
  #_(com.sun.jna.NativeLibrary/getInstance "clang"))

(def clang-api (with-open [rdr (io/reader
                                (io/resource
                                 "com/phronemophobic/clong/clang/api.edn"))
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

(gen/def-api libclang clang-api)



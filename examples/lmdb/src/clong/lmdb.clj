(ns com.phronemophobic.lmdb
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
   com.sun.jna.ptr.IntByReference
   com.sun.jna.Structure)
  (:gen-class))

(def ^:no-doc liblmdb
  (com.sun.jna.NativeLibrary/getInstance "lmdb"))

(def api (clong/easy-api "/opt/local/include/lmdb.h"))

(gen/def-api liblmdb api)
(gen/import-structs! api)

(let [major (IntByReference.)
      minor (IntByReference.)
      patch (IntByReference.)]
 (mdb_version major minor patch)
 (mapv #(.getValue %) [major minor patch])) ;; [0 9 29]


(def env* (PointerByReference.))
(mdb_env_create env*) ;; 0
(def env (.getValue env*))

(def db-file (io/file "db"))
(.mkdirs db-file)
(def db-path (.getAbsolutePath db-file))

(def MDB_FIXEDMAP	0x01)

(mdb_env_open env
              "./db"
              MDB_FIXEDMAP
              0644) ;; 0

;; (mdb_env_close (.getValue env))

(def txn* (PointerByReference.) )
(mdb_txn_begin env nil 0 txn*) ;; 0

(def dbi* (IntByReference.))
(mdb_dbi_open (.getValue txn*), nil, 0, dbi*);; 0

(def tx-key (MDB_valByReference.))
(def key-bytes (.getBytes "clong" "utf-8"))
(def key-mem
  (doto (Memory. (inc (alength key-bytes)))
    (.write 0 key-bytes 0 (alength key-bytes))
    (.setByte (alength key-bytes) 0)))
(set! (.mv_data tx-key) key-mem)
(set! (.mv_size tx-key) (.size key-mem))
(.write tx-key)

(def tx-val (MDB_valByReference.))
(def num* (IntByReference.))
(.setValue num* 42)

(set! (.mv_size tx-val) 4)
(set! (.mv_data tx-val) (.getPointer num*))
(.write tx-val)


(mdb_put (.getValue txn*)
         (.getValue dbi*)
         tx-key
         tx-val
         0) ;; 0

(mdb_txn_commit (.getValue txn*)) ;; 0

(def mst* (MDB_statByReference.))
(mdb_env_stat env mst*)
(.ms_entries mst*) ;; 1

(mdb_txn_begin env nil 0 txn*) ;; 0

(def tx-ret (MDB_valByReference.))
(mdb_get (.getValue txn*)
         (.getValue dbi*)
         tx-key
         tx-ret)

(.read tx-ret)
(.getInt (.mv_data tx-ret) 0) ;; 42


(mdb_dbi_close env (.getValue dbi*))
(mdb_env_close env)

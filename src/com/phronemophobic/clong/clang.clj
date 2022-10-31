(ns com.phronemophobic.clong.clang
  (:require [com.phronemophobic.clong.clang.raw :as c]
            [coffi.mem :as mem :refer [defalias]]))

(def null-pointer
  (doto (mem/alloc-instance ::mem/pointer)
    (mem/write-long 0)))

(defn get-children [cursor]
  (let [childs (volatile! [])
        visitor (fn [child parent _]
                  (vswap! childs conj child)
                  1)]
    (c/clang_visitChildren cursor
                           visitor
                           null-pointer)
    @childs))

(defonce handles (atom #{}))
(defn parse [fname args]
  (let [scope (mem/global-scope)

        index (c/clang_createIndex 0 0)

        cmd-args (if (seq args)
                   (mem/serialize args
                                  [::mem/array ::mem/c-string (count args)]
                                  scope)
                   ;; else
                   null-pointer)
        
        translation-unit (c/clang_parseTranslationUnit index fname cmd-args (count args) null-pointer 0 0)
        cursor (c/clang_getTranslationUnitCursor translation-unit)]
    (swap! handles conj translation-unit index cmd-args)
    cursor))

(def ^:private cursor-kinds*
  (->> (:enums c/clang-api)
       (filter #(= "CXCursorKind"
                   (:enum %)))
       (into {}
             (map (fn [e]
                    [(:value e)
                     (:name e)])))))

(defn cursor-kind->str [n]
  (cursor-kinds* n))

(def ^:private cursor-type-kinds*
  (->> (:enums c/clang-api)
       (filter #(= "CXTypeKind"
                   (:enum %)))
       (into {}
             (map (fn [e]
                    [(:value e)
                     (:name e)])))))

(defn cursor-type-kind->str [n]
  (cursor-type-kinds* n))


(defmulti extra-cursor-info (fn [cur]
                              (cursor-kind->str (:kind cur))))


(defmethod extra-cursor-info :default
  [cur]
  {})

(defn cursor-info [cur]
  (merge
   {:kind (cursor-kind->str (:kind cur))
    :type (cursor-type-kind->str (:kind (c/clang_getCursorType cur)))}
   (extra-cursor-info cur)))


(defn enum-decl? [cur]
  (= "CXCursor_EnumDecl"
     (:kind cur)))



(defmethod extra-cursor-info "CXCursor_EnumDecl"
  [cur]
  {:spelling (c/clang_getCString
              (c/clang_getCursorSpelling cur))
   :raw-comment (c/clang_getCString
                 (c/clang_Cursor_getRawCommentText cur))})

(defn function-decl? [cur]
  (= "CXCursor_FunctionDecl"
     (:kind cur)))

(defmethod extra-cursor-info "CXCursor_FunctionDecl"
  [cur]
  {:spelling (c/clang_getCString
              (c/clang_getCursorSpelling cur))
   :raw-comment (c/clang_getCString
                 (c/clang_Cursor_getRawCommentText cur))})

(comment

  (def cursor (parse
               "/Users/adrian/workspace/llvm-project/build/out/include/clang-c/Index.h"
               ["-I/Users/adrian/workspace/llvm-project/build/out/include/"]))

  (def tree
    (doall
     (->> (tree-seq (constantly true)
                    get-children
                    cursor)
          (map cursor-info))))

  (def fns (->> tree
                (filter function-decl?)))

  (def enums
    (->> tree
         (filter enum-decl?)))
  ,)








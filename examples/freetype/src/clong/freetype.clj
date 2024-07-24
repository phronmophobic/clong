(ns com.phronemophobic.freetype
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [com.phronemophobic.clong.clang :as clang]
            [com.phronemophobic.clong.gen.jna :as gen])
  (:import
   java.io.PushbackReader
   com.sun.jna.Memory
   com.sun.jna.Pointer
   com.sun.jna.ptr.PointerByReference
   com.sun.jna.ptr.LongByReference
   com.sun.jna.Structure)
  (:gen-class))

(def ^:no-doc libfreetype
  (com.sun.jna.NativeLibrary/getInstance "freetype"))

(def api (clang/easy-api "/opt/local/include/freetype2/freetype/freetype.h"
                         ["-I/opt/local/include/freetype2/"]))

(gen/def-api libfreetype api)
(gen/import-structs! api)

(def library* (PointerByReference.))

(FT_Init_FreeType library* ) ;; 0
(def library (.getValue library*))

(def face* (PointerByReference.))

(FT_New_Face library
             "/System/Library/Fonts/Supplemental/Comic Sans MS.ttf"
             0
             face*
             ) ;; 0
(def face (.getValue face*))

(def face+ (doto (Structure/newInstance FT_FaceRec_
                                        face)
             (.read)))

(.num_glyphs face+) ;; 587
(.units_per_EM face+) ;; 2048
(.available_sizes face+);;

(FT_Set_Char_Size face, ;;   /* handle to face object           */
                  0,    ;;   /* char_width in 1/64th of points  */
                  (* 16 64),;;   /* char_height in 1/64th of points */
                  300,  ;;   /* horizontal device resolution    */
                  300    ;;   /* vertical device resolution      */
                  );; 0

(def glyph-index (FT_Get_Char_Index face, \a  )) ;; 68

(FT_Load_Glyph
 face, ;;         /* handle to face object */
 glyph-index,;;   /* glyph index           */
 0 );; 0

(FT_Render_Glyph (.glyph face+),  ; /* glyph slot  */
                 FT_RENDER_MODE_NORMAL)

(def bitmap (doto (-> face+
                      .glyph
                      .bitmap)
              (.read)))

(.rows bitmap) ;; 35
(.width bitmap) ;; 33






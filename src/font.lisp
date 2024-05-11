(defpackage :dungeon/font
  (:use :cl dungeon/globals)
  (:export #:init-font
           #:clear-font))
(in-package :dungeon/font)

(defun init-font ()
  (setf *glyph-font* (sdl2-ttf:open-font *glyph-font-path* *glyph-size*))
  (setf *text-font* (sdl2-ttf:open-font *text-font-path* *glyph-size*)))

(defun clear-font ()
  (sdl2-ttf:close-font *glyph-font*)
  (sdl2-ttf:close-font *text-font*))

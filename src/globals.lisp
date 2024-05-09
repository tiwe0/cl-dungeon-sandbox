(defpackage :dungeon/globals
  (:use :cl))
(in-package :dungeon/globals)

;; screen
(defparameter *screen-width* 800)
(defparameter *screen-height* 600)

;; glyph
(defparameter *glyph-size* 32)
(defparameter *glyph-font* nil)
(defparameter *glyph-font-path* (asdf:system-relative-pathname 'dungeon "assets/fonts/PressStart2P-Regular.ttf"))
(defparameter *glyph-array* (make-array 20 :adjustable t :fill-pointer t))
(defparameter *glyph-texture* (make-hash-table))
(defparameter *glyph-chars* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ·!@#$%^&*()_+-=,.<>/?")

;; camera
(defparameter *camera-main* nil)


(let ((pack (find-package :dungeon/globals)))
  (do-all-symbols (sym pack)
    (when (eql (symbol-package sym) pack)
      (export sym))))

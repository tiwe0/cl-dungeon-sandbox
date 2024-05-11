(defpackage :dungeon/globals
  (:use :cl)
  (:export #:*screen-width*
           #:*screen-height*
           #:*glyph-size*
           #:*glyph-font*
           #:*glyph-font-path*
           #:*glyph-array*
           #:*glyph-texture*
           #:*glyph-chars*
           #:*camera-main*
           #:*gamemap-main*
           #:*should-render*
           #:*current-ticks*
           #:*current-delta*
           #:*text-font-path*
           #:*text-font*
           #:*text-array*))
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

;; gamemap
(defparameter *gamemap-main nil)

;; render
(defparameter *should-render* t)

;; timer
(defparameter *current-ticks* nil)
(defparameter *current-delta* nil)

;; font
(defparameter *text-font-path* (asdf:system-relative-pathname 'dungeon "assets/fonts/PixelTextFont-Regular.ttf"))
(defparameter *text-font* nil)

(defparameter *text-array* (make-array 20 :adjustable t :fill-pointer 0))


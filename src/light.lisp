(defpackage :dungeon/light
  (:use :cl :dungeon/color))
(in-package :dungeon/light)

(defstruct light-info
  intensity
  color
  z)

(defparameter *light-array* (make-array 10 :adjustable t :fill-pointer t))

(defclass light ()
  ((light-radius :initarg :light-radius :initform 5)
   (light-intensity :initarg :light-intensity :initform 120)
   (light-color :initarg :light-color :initform +white+)
   (light-fade :initarg :light-fade :initform t)
   (light-shadow :initarg :light-shadow :initform t)
   (light-game-position: initarg :light-game-position)))

(defmethod light-compute-intensity ((light light) (glyph-game-position vec2)))

(defmethod light-apply ((light light)) (glyph-array array)
  ())

(defmethod initialize-instance :after ((light light))
  (vector-push light *light-array*))


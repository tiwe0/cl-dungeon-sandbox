(defpackage :cl-dungeon
  (:use :cl #:cl-dungeon/utils))
(in-package :cl-dungeon)

(defvar *viewport-width* 800)
(defvar *viewport-height* 600)
(defvar *screen-width* 800)
(defvar *screen-height* 600)

(prin1 (make-instance 'vect2
         :x 2
         :y 9))

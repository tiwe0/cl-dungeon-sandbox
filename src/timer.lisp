(defpackage :dungeon/timer
  (:use :cl dungeon/globals)
  (:export #:update-delta))
(in-package :dungeon/timer)

(defun update-delta ()
  (let ((current-ticks (sdl2:get-ticks)))
    (setf *current-delta* (/ (- current-ticks *current-ticks*) 1000))
    (setf *current-ticks* current-ticks)))


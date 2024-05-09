(defpackage :dungeon/test
  (:use :cl))
(in-package :dungeon/test)

(sdl2:make-this-thread-main #'dungeon:main)


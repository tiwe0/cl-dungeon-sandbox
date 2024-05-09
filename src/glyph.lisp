(defpackage :dungeon/glyph
  (:use :cl :3d-vectors :dungeon/globals)
  (:export #:glyph
           #:init-glyph-texture
           #:clear-glyph-texture))
(in-package :dungeon/glyph)

(defclass glyph ()
  ((glyph-string :accessor glyph-string :initarg :glyph-string :initform "····*")
   (glyph-position :accessor glyph-position :initarg :glyph-position :initform (vec3 0 0 0))))

(defmethod initialize-instance :after ((glyph glyph) &rest initargs)
  (format t "glyph has been created.~%")
  (vector-push-extend glyph *glyph-array*))

(defun init-glyph-texture (renderer)
  "初始化glyph材质"
  (loop :for c :across dungeon/globals::*glyph-chars*
        :do (when (not (gethash c dungeon/globals::*glyph-texture*))
              (format t "try add glyph texture: ~a~%" c)
              (let* ((surface (sdl2-ttf:render-utf8-blended dungeon/globals::*glyph-font* (string c) #x0 #x0 #x0 #xFF))
                     (texture (sdl2:create-texture-from-surface renderer surface)))
                (setf (gethash c dungeon/globals::*glyph-texture*) texture)
                ;;(sdl2:free-surface surface)
                ))))

(defun clear-glyph-texture ()
  "清理glyph材质"
  (loop :for c :across *glyph-chars* :do
    (when (gethash c *glyph-texture*))
    (format t "try clear glyph texture: ~a" c)
    (sdl2:destroy-texture (gethash *glyph-texture* c))))

(defvar test-glyph-1 (make-instance 'glyph :glyph-position (vec3 80 80 0)))
(defvar test-glyph-2 (make-instance 'glyph :glyph-position (vec3 0 0 0)))

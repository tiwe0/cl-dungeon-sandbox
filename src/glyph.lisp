(defpackage :dungeon/glyph
  (:use :cl :3d-vectors :dungeon/globals)
  (:export #:glyph
           #:init-glyph-texture
           #:clear-glyph-texture))
(in-package :dungeon/glyph)

(defclass glyph ()
  ((glyph-string :accessor glyph-string :initarg :glyph-string :initform "····*")
   (glyph-color :accessor glyph-color :initarg :glyph-color :initform (vec3 0 0 0))
   (glyph-position :accessor glyph-position :initarg :glyph-position :initform (vec3 0 0 0))))

(defmethod initialize-instance :after ((glyph glyph) &rest initargs)
  (format t "glyph has been created.~%")
  (vector-push-extend glyph *glyph-array*))

(defmethod glyph-char-color ((glyph glyph) (index number))
  (with-slots (glyph-color) glyph
    (if (or (typep glyph-color 'vec3) (>= index (length glyph-color)))
        glyph-color
        (nth index glyph-color))))

(defun init-glyph-texture (renderer)
  "初始化glyph材质"
  (loop :for c :across dungeon/globals::*glyph-chars*
        :do (when (not (gethash c dungeon/globals::*glyph-texture*))
              (format t "try add glyph texture: ~a~%" c)
              (let* ((surface (sdl2-ttf:render-utf8-blended dungeon/globals::*glyph-font* (string c) #xFF #xFF #xFF #xFF))
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
(defvar test-glyph-2 (make-instance 'glyph :glyph-position (vec3 0 0 0) :glyph-color (vec3 0 255 0)))

(defvar test-glyph-3 (make-instance 'glyph :glyph-position (vec3 -80 80 0) :glyph-color (list (vec3 255 0 0)
                                                                                              (vec3 0 255 0)
                                                                                              (vec3 0 0 255)
                                                                                              (vec3 255 255 0)
                                                                                              (vec3 255 0 255))))

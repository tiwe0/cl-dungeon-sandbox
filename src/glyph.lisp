(defpackage :dungeon/glyph
  (:use :cl :3d-vectors dungeon/globals dungeon/color)
  (:export #:glyph
           #:init-glyph-texture
           #:clear-glyph-texture))
(in-package :dungeon/glyph)

(defclass glyph ()
  ((glyph-string :accessor glyph-string :initarg :glyph-string :initform "····*")
   (glyph-color :accessor glyph-color :initarg :glyph-color :initform (vec3 0 0 0))
   (glyph-game-position :accessor glyph-game-position :initarg :glyph-game-position :initform (vec2 0 0))
   (glyph-gamemap :accessor glyph-gamemap :initarg :glyph-gamemap :initform nil)
   (glyph-light-color :accessor glyph-light-color)
   (glyph-light-tensity :accessor glyph-light-tensity)))

(defmethod glyph-depth ((glyph glyph) (index number))
  (let ((the-glyph-length (glyph-length glyph)))
    (- the-glyph-length index 1)))

(defmethod glyph-length ((glyph glyph))
  (with-slots (glyph-string) glyph
    (length glyph-string)))

(defmethod glyph-position ((glyph glyph))
  (with-slots (glyph-game-position) glyph
    (let ((render-position (v* *glyph-size* glyph-game-position)))
      (v- (vec3 (vx render-position) (vy render-position) 0) (vec3 (/ *glyph-size* 2) (/ *glyph-size* 2) 0)))))

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


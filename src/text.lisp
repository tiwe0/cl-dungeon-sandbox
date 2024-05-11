(defpackage :dungeon/text
  (:use :cl :3d-vectors dungeon/globals dungeon/color)
  (:export #:make-dungeon-text
           #:render
           #:render-text))
(in-package :dungeon/text)

(defstruct +dungeon-text+
  text
  show
  position
  color)

(defun make-dungeon-text (text position show color)
  (let ((the-text (make-+dungeon-text+ :text text :show show :position position :color color)))
    (vector-push the-text *text-array*)
    the-text))

(defun render-text (renderer text position color)
  (let* ((text-surface (sdl2-ttf:render-utf8-blended *text-font*
                                                     text
                                                     (round (vx color))
                                                     (round (vy color))
                                                     (round (vz color))
                                                     (round (vw color))))
         (dest-rect (sdl2:make-rect (round (vx position))
                                    (round (vy position))
                                    (sdl2:surface-width text-surface)
                                    (sdl2:surface-height text-surface)))
         (text-texture (sdl2:create-texture-from-surface renderer text-surface)))
    (sdl2:render-copy renderer text-texture :dest-rect dest-rect)
    (sdl2:destroy-texture text-texture)))

(defun render (renderer text-struct)
  (with-slots (text color position show) text-struct
    (when show
      (render-text renderer text position color))))

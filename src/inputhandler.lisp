(defpackage :dungeon/inputhandler
  (:use :cl :3d-vectors dungeon/globals dungeon/timer)
  (:export #:handle-key-down-event
           #:handle-key-up-event
           #:handle-quit-event
           #:handle-mouse-event
           #:handle-idle-event))
(in-package :dungeon/inputhandler)

(defmacro clearall ()
  ;; clear glyph texture
  `(loop :for c :across *glyph-chars* :do
    (when (gethash c *glyph-texture*))
    (format t "try clear glyph texture: ~a" c)
    (sdl2:destroy-texture (gethash *glyph-texture* c)))
  `(sdl2-ttf:close-font *glyph-font*)
  `(sdl2-ttf:quit))

(defun handle-quit-event ()
  (format t "~a~%" "bye~")
  (clearall)
  (sb-ext:exit))

(defun handle-key-up-event (keysym)
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
    (sdl2:push-event :quit)))

(defun handle-key-down-event (keysym)
  (let ((scancode (sdl2:scancode-value keysym))
        (sym (sdl2:sym-value keysym))
        (mod-value (sdl2:mod-value keysym)))
    (update-delta)
    (cond
      ((sdl2:scancode= scancode :scancode-w)
       (progn
         (format t "~a~%" "go north")
         (dungeon/camera:camera-game-position- *camera-main* (vec 0 1 0))))
      ((sdl2:scancode= scancode :scancode-a)
       (progn
         (format t "~a~%" "go west")
         (dungeon/camera:camera-game-position- *camera-main* (vec 1 0 0))))
      ((sdl2:scancode= scancode :scancode-s)
       (progn
         (format t "~a~%" "go sourth")
         (dungeon/camera:camera-game-position+ *camera-main* (vec 0 1 0))))
      ((sdl2:scancode= scancode :scancode-d)
       (progn
         (format t "~a~%" "go east")
         (dungeon/camera:camera-game-position+ *camera-main* (vec 1 0 0))))
      ((sdl2:scancode= scancode :scancode-q)
       (progn
         (format t "~a~%" "go up")
         (dungeon/camera:camera-game-position+ *camera-main* (vec 0 0 1))))
      ((sdl2:scancode= scancode :scancode-e)
       (progn
         (format t "~a~%" "go down")
         (dungeon/camera:camera-game-position- *camera-main* (vec 0 0 1))))
      )
    (format t "camera position: ~a~%" (dungeon/camera::camera-position *camera-main*))
    (format t "Key sym: ~a, code: ~a, mod: ~a~%" sym scancode mod-value))
  )

(defun handle-mouse-event (x y xrel yrel state)
  (format t "Mouse motion abs(rel): ~a(~a), ~a(~a); Mouse State: (~a);~%"
          x xrel y yrel state)
  (format t "Mouse game position: ~a~%" (dungeon/camera:camera-get-mouse-game-position *camera-main*)))

(defun handle-idle-event (renderer)
  (update-delta)
  (when *should-render*
    (progn
      (sdl2:set-render-draw-color renderer #x00 #x00 #x00 #xFF)
      (sdl2:render-clear renderer)
      (dungeon/camera:camera-render-mapgrid *camera-main*)
      (dungeon/camera:camera-render-target-axis *camera-main*)
      (dungeon/camera:camera-render-gamemap *camera-main* *gamemap-main*)
      (dungeon/camera::camera-viewport-render-char *camera-main* #\@ (vec2 0 0))
      (dungeon/camera:camera-render-text-array *camera-main*)
      (dungeon/text:render-text renderer (format nil "~a" (dungeon/camera:camera-get-mouse-game-position *camera-main*)) (vec2 4 4) dungeon/color:+yellow+)
      (sdl2:render-present renderer))))


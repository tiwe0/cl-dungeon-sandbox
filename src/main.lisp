(defpackage :dungeon
  (:use :cl :3d-vectors :dungeon/globals)
  (:export #:main))
(in-package :dungeon)

(defmacro clearall ()
  ;; clear glyph texture
  `(loop :for c :across *glyph-chars* :do
    (when (gethash c *glyph-texture*))
    (format t "try clear glyph texture: ~a" c)
    (sdl2:destroy-texture (gethash *glyph-texture* c)))
  `(sdl2-ttf:close-font *glyph-font*)
  `(sdl2-ttf:quit))

(defparameter *should-render* t)
(defun render ()
  (setf *should-render* t))

(defparameter *current-ticks* nil)
(defparameter *current-delta* nil)

(defun test-glyph ()
  (defvar test-glyph-1 (make-instance 'dungeon/glyph:glyph :glyph-game-position (vec2 0 0) :glyph-color (vec3 255 255 255) :glyph-gamemap *gamemap-main*))
  (defvar test-glyph-2 (make-instance 'dungeon/glyph:glyph :glyph-game-position (vec2 0 4) :glyph-color (vec3 0 255 0) :glyph-gamemap *gamemap-main*))
  (defvar test-glyph-3 (make-instance 'dungeon/glyph:glyph :glyph-game-position (vec2 6 0) :glyph-gamemap *gamemap-main* :glyph-color (list dungeon/color::+brown+
                                                                                                dungeon/color::+brown+
                                                                                                dungeon/color::+brown+
                                                                                                dungeon/color::+brown+
                                                                                                (vec3 0 255 0)))))

(defun update-delta ()
  (let ((current-ticks (sdl2:get-ticks)))
    (setf *current-delta* (/ (- current-ticks *current-ticks*) 1000))
    (setf *current-ticks* current-ticks)))

(defun main ()
  ;; init sdl2
  (sdl2:with-init (:video)
    (sdl2-ttf:init)
    (sdl2:with-window (window :title "DUNGEON" :w 800 :h 600)
      (sdl2:with-renderer (renderer window :index -1 :flags '(:accellerated))
        ;; 加载字体
        (dungeon/font:init-font)
        ;; 初始化字符纹理
        (dungeon/glyph:init-glyph-texture renderer)
        ;; 创建camera
        (setf *camera-main* (make-instance 'dungeon/camera:camera
                                           :camera-renderer renderer
                                           :camera-viewport-size (vec2 800 600)
                                           :camera-position (vec3 0 0 800)
                                           :camera-target-position (vec3 0 0 0)))
        ;; 创建游戏地图
        (setf *gamemap-main* (make-instance 'dungeon/gamemap:gamemap))
        ;; 创建glyph
        (test-glyph)
        ;; 初始化ticks
        (setf *current-ticks* (sdl2:get-ticks))
        (update-delta)
        (sdl2:with-event-loop (:method :poll)
          (:quit () (progn
                      (format t "~a~%" "bye~")
                      (clearall)
                      (sb-ext:exit)))
          (:keydown (:keysym keysym)
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
                      (format t "Key sym: ~a, code: ~a, mod: ~a~%" sym scancode mod-value)))
          (:keyup (:keysym keysym)
                  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                    (sdl2:push-event :quit)))
          (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
                        (progn
                          (format t "Mouse motion abs(rel): ~a(~a), ~a(~a); Mouse State: (~a);~%"
                                  x xrel y yrel state)
                          (format t "Mouse game position: ~a~%" (dungeon/camera:camera-get-mouse-game-position *camera-main*))))
          (:idle ()
                 (progn
                   (update-delta)
                   (when *should-render*
                     (progn
                       (sdl2:set-render-draw-color renderer #x00 #x00 #x00 #xFF)
                       (sdl2:render-clear renderer)
                       (dungeon/camera:camera-render-mapgrid *camera-main*)
                       (dungeon/camera:camera-render-target-axis *camera-main*)
                       (dungeon/camera:camera-render-gamemap *camera-main* *gamemap-main*)
                       (dungeon/camera::camera-viewport-render-char *camera-main* #\@ (vec2 0 0))
                       (sdl2:render-present renderer))))))))))


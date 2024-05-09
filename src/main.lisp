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

(defun main ()
  ;; init sdl2
  (sdl2:with-init (:video)
    (sdl2-ttf:init)
    (sdl2:with-window (window :title "DUNGEON" :w 800 :h 600)
      (sdl2:with-renderer (renderer window :index -1 :flags '(:accellerated))
        ;; 加载字体
        (dungeon/font:init-font)
        ;; 创建camera
        (setf *camera-main* (make-instance 'dungeon/camera:camera
                                           :camera-renderer renderer
                                           :camera-viewport-size (vec2 800 600)
                                           :camera-position (vec3 0 0 800)
                                           :camera-target-position (vec3 0 0 0)))
        ;; 初始化字符纹理
        (dungeon/glyph:init-glyph-texture renderer)
        (sdl2:with-event-loop (:method :poll)
          (:quit () (progn
                      (format t "~a~%" "bye~")
                      (clearall)
                      (sb-ext:exit)))
          (:keydown (:keysym keysym)
                    (let ((scancode (sdl2:scancode-value keysym))
                          (sym (sdl2:sym-value keysym))
                          (mod-value (sdl2:mod-value keysym)))
                      (cond
                        ((sdl2:scancode= scancode :scancode-w)
                         (progn
                           (format t "~a~%" "go north")
                           (dungeon/camera:camera-all-position- *camera-main* (vec 0 *glyph-size* 0))))
                        ((sdl2:scancode= scancode :scancode-a)
                         (progn
                           (format t "~a~%" "go west")
                           (dungeon/camera:camera-all-position- *camera-main* (vec *glyph-size* 0 0))))
                        ((sdl2:scancode= scancode :scancode-s)
                         (progn
                           (format t "~a~%" "go sourth")
                           (dungeon/camera:camera-all-position+ *camera-main* (vec 0 *glyph-size* 0))))
                        ((sdl2:scancode= scancode :scancode-d)
                         (progn
                           (format t "~a~%" "go east")
                           (dungeon/camera:camera-all-position+ *camera-main* (vec *glyph-size* 0 0))))
                        ((sdl2:scancode= scancode :scancode-q)
                         (progn
                           (format t "~a~%" "go up")
                           (dungeon/camera:camera-all-position+ *camera-main* (vec 0 0 *glyph-size*))))
                        ((sdl2:scancode= scancode :scancode-e)
                         (progn
                           (format t "~a~%" "go down")
                           (dungeon/camera:camera-all-position- *camera-main* (vec 0 0 *glyph-size*))))
                        )
                      (format t "camera position: ~a~%" (dungeon/camera::camera-position *camera-main*))
                      (format t "Key sym: ~a, code: ~a, mod: ~a~%" sym scancode mod-value)))
          (:keyup (:keysym keysym)
                  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                    (sdl2:push-event :quit)))
          (:idle ()
                 (when *should-render*
                   (progn
                     (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
                     (sdl2:render-clear renderer)
                     (dungeon/camera:camera-render-glyph-array *camera-main* *glyph-array*)
                     (dungeon/camera:camera-render-target-axis *camera-main*)
                     (sdl2:render-present renderer)))))))))


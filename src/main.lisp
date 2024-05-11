(defpackage :dungeon
  (:use :cl :3d-vectors dungeon/globals dungeon/inputhandler dungeon/timer)
  (:export #:main))
(in-package :dungeon)

(defun render ()
  (setf *should-render* t))

(defun test-glyph ()
  (defvar test-glyph-1 (make-instance 'dungeon/glyph:glyph :glyph-game-position (vec2 0 0) :glyph-color (vec3 255 255 255) :glyph-gamemap *gamemap-main*))
  (defvar test-glyph-2 (make-instance 'dungeon/glyph:glyph :glyph-game-position (vec2 0 4) :glyph-color (vec3 0 255 0) :glyph-gamemap *gamemap-main*))
  (defvar test-glyph-3 (make-instance 'dungeon/glyph:glyph :glyph-game-position (vec2 6 0) :glyph-gamemap *gamemap-main* :glyph-color (list dungeon/color::+brown+
                                                                                                dungeon/color::+brown+
                                                                                                dungeon/color::+brown+
                                                                                                dungeon/color::+brown+
                                                                                                (vec3 0 255 0)))))

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
          (:quit        ()                                             (handle-quit-event))
          (:keydown     (:keysym keysym)                               (handle-key-down-event keysym))
          (:keyup       (:keysym keysym)                               (handle-key-up-event keysym))
          (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state) (handle-mouse-event x y xrel yrel state))
          (:idle        ()                                             (handle-idle-event renderer)))))))


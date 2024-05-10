(defpackage :dungeon/camera
  (:use :cl :3d-vectors :dungeon/glyph :dungeon/globals)
  (:export #:camera
           #:camera-render-target-axis
           #:camera-render-glyph
           #:camera-render-glyph-array
           #:camera-position-setf
           #:camera-target-position-setf
           #:camera-position+
           #:camera-position-
           #:camera-target-position+
           #:camera-target-position-
           #:camera-all-position+
           #:camera-all-position-))
(in-package :dungeon/camera)

(defclass camera ()
  ((camera-renderer :initarg :camera-renderer :accessor camera-renderer)
   (camera-viewport-size :initarg :camera-viewport-size :accessor camera-viewpor-size)
   (camera-position :initarg :camera-position :accessor camera-position)
   (camera-target-position :initarg :camera-target-position :accessor camera-target-position)))

(defmethod camera-viewport-origin-position ((camera camera))
  "计算viewport的原点位置"
  (with-slots (camera-target-position camera-viewport-size) camera
    (let* ((half-viewport-size (v/ camera-viewport-size 2))
           (viewport-origin-position (v- (vxy camera-target-position) half-viewport-size)))
      viewport-origin-position)))

(defmethod camera-compute-viewport-position ((camera camera) (position vec3))
  "计算目标相对于viewport原点的位置，也就是渲染在窗口的位置"
  (let* ((viewport-origin-position (camera-viewport-origin-position camera))
         (relative-to-viewport-position (v- (vxy position) viewport-origin-position)))
    relative-to-viewport-position))

(defmethod camera-compute-shadowcasting-position ((camera camera) (target-position vec3) &optional (base 0))
  "计算目标的投影位置"
  (let* ((pivot-position (camera-position camera))
         (pivot-height (- (vz pivot-position) base))
         (target-height (- (vz target-position) base))
         (ratio (/ target-height pivot-height))
         (shadow-x (/ (- (vx target-position) (* ratio (vx pivot-position))) (- 1 ratio)))
         (shadow-y (/ (- (vy target-position) (* ratio (vy pivot-position))) (- 1 ratio)))
         (shadow-position (vec3 shadow-x shadow-y base)))
    shadow-position))

;; 临时修改材质颜色
(defmacro with-texture-color-change (texture color &body body)
  `(progn
     (sdl2:set-texture-color-mod ,texture (round (vx ,color)) (round (vy ,color)) (round (vz ,color)))
     ,@body
     (sdl2:set-texture-color-mod ,texture #xFF #xFF #xFF)))

;; 注意这里是 vec2 （使用默认颜色）
(defmethod camera-viewport-render-char ((camera camera) (char character) (position vec2) &optional (color (vec3 0 0 0)))
  "渲染单个char"
  (with-slots (camera-renderer) camera
    (let* ((x (round (vx position)))
           (y (round (vy position)))
           (dest-rect (sdl2:make-rect x y *glyph-size* *glyph-size*))
           (texture (gethash char *glyph-texture*)))
      ;; 修改为指定颜色
      (with-texture-color-change texture color
        (sdl2:render-copy camera-renderer texture :source-rect (cffi:null-pointer) :dest-rect dest-rect)))))

;; 注意这里是 vec3
(defmethod camera-render-char ((camera camera) (char character) (position vec3) &optional (color (vec3 0 0 0)))
  "渲染单个char"
  (let* ((shadowcasting-position (camera-compute-shadowcasting-position camera position))
         (render-position (camera-compute-viewport-position camera shadowcasting-position)))
    (camera-viewport-render-char camera char render-position color)))

(defmethod camera-render-target-axis ((camera camera))
  "渲染camera的准星"
  (camera-render-char camera #\+ (camera-target-position camera) (vec3 #xFF 0 0)))

;; TODO 不在viewport内部的glyph不渲染
(defmethod camera-render-glyph ((camera camera) (glyph glyph))
  "渲染glyph"
  (loop :for index :from 0
        :for c :across (dungeon/glyph::glyph-string glyph)
        :do (let ((glyph-char-position (v+ (dungeon/glyph::glyph-position glyph) (v* index (vec3 0 0 *glyph-size*))))
                  (glyph-char-color (dungeon/glyph::glyph-char-color glyph index)))
              (camera-render-char camera c glyph-char-position glyph-char-color))))

(defmethod camera-render-glyph-array ((camera camera) (glyph-array array))
  "渲染全部glyph"
  (loop :for glyph :across glyph-array
        :do (when (typep glyph 'glyph)
              (camera-render-glyph camera glyph))))

(defmethod camera-position-setf ((camera camera) (new-position vec3))
  (with-slots (camera-position) camera
    (setf camera-position new-position)))

(defmethod camera-target-position-setf ((camera camera) (new-position vec3))
  (with-slots (camera-position) camera
    (setf camera-target-position new-position)))

(defmethod camera-position+ ((camera camera) (offset vec3))
  (with-slots (camera-position) camera
    (nv+ camera-position offset)))

(defmethod camera-position- ((camera camera) (offset vec3))
  (with-slots (camera-position) camera
    (nv- camera-position offset)))

(defmethod camera-target-position+ ((camera camera) (offset vec3))
  (with-slots (camera-target-position) camera
    (nv+ camera-target-position offset)))

(defmethod camera-target-position- ((camera camera) (offset vec3))
  (with-slots (camera-target-position) camera
    (nv- camera-target-position offset)))

(defmethod camera-all-position+ ((camera camera) (offset vec3))
  (camera-target-position+ camera offset)
  (camera-position+ camera offset))

(defmethod camera-all-position- ((camera camera) (offset vec3))
  (camera-target-position- camera offset)
  (camera-position- camera offset))

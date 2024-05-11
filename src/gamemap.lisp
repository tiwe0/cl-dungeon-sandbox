(defpackage :dungeon/gamemap
  (:use :cl :3d-vectors :dungeon/glyph)
  (:export #:gamemap
           #:add-glyph
           #:gamemap-glyph-array))
(in-package :dungeon/gamemap)

(defclass gamemap ()
  ((gamemap-size :initarg :gamemap-size :initform (vec2 30 18) :accessor gamemap-size)
   (gamemep-glyph-size :initarg :glyph-size :initform 32 :accessor gamemap-glyph-size)
   (gamemap-glyph-array :initform (make-array 20 :adjustable t :fill-pointer 0) :accessor gamemap-glyph-array)
   (gamemap-mapcell :initform nil :accessor gamemap-mapcell)))

(defmethod initialize-instance :after ((gamemap gamemap) &rest initarg)
  (with-slots (gamemap-mapcell gamemap-size) gamemap
    (setf gamemap-mapcell (make-hash-table :test #'equal))))

(defmethod has-mapcell-at-yx ((gamemap gamemap) (game-position vec2))
  (with-slots (gamemap-mapcell) gamemap
    (not (null (gethash (list (vy game-position) (vx game-position)) gamemap-mapcell)))))

(defmethod init-mapcell-at-yx ((gamemap gamemap) (game-position vec2))
  (when (not (has-mapcell-at-yx gamemap game-position))
    (with-slots (gamemap-mapcell) gamemap
      (format t "[gamemap]: init mapcell at xy: ~a~%" game-position)
      (setf (gethash (list (vy game-position) (vx game-position)) gamemap-mapcell)
            (make-array 20 :adjustable t :fill-pointer 0))
      (format t "[gamemap]: mapcell inited: ~a~%" (gethash (list (vy game-position) (vx game-position)) gamemap-mapcell)))))

(defmethod destroy-mapcell-at-yx ((gamemap gamemap) (game-position vec2))
  ())

(defmacro with-mapcell-at-yx-ensure (gamemap game-position mapcell &body body)
  `(progn
     (init-mapcell-at-yx ,gamemap ,game-position)
     (format t "[gamemap]: access mapcell at xy: ~a~%" ,game-position)
     (with-slots (gamemap-mapcell) ,gamemap
       (let ((,mapcell (gethash (list (vy ,game-position) (vx ,game-position)) gamemap-mapcell)))
         ,@body))))

(defmacro with-mapcell-at-yx (gamemap game-position mapcell &body body)
  `(progn
     (with-slots (gamemap-mapcell) ,gamemap
       (let ((,mapcell (gethash (list (vy ,game-position) (vx ,game-position)) gamemap-mapcell)))
         (when ,mapcell
           ,@body)))))

(defmethod add-glyph-to-array ((gamemap gamemap) (glyph glyph))
  (with-slots (gamemap-glyph-array) gamemap
    (vector-push glyph gamemap-glyph-array)
    (format t "[gamemap] glyph-added-to-array: ~a~%" gamemap-glyph-array)))

(defmethod add-glyph-to-mapcell ((gamemap gamemap) (glyph glyph))
  (let ((game-position (dungeon/glyph::glyph-game-position glyph)))
    (format t "[gamemap] add-glyph-to-mapcell~%")
    (with-mapcell-at-yx-ensure gamemap game-position mapcell
      (vector-push glyph mapcell))))

(defmethod add-glyph ((gamemap gamemap) (glyph glyph))
  (format t "[gamemap] add-glyph~%")
  (add-glyph-to-array gamemap glyph)
  (add-glyph-to-mapcell gamemap glyph))

(defmethod remove-glyph ((gamemap gamemap) (glyph glyph))
  (remove-glyph-from-array gamemap glyph)
  (remove-glyph-from-mapcell gamemap glyph))

(defmethod remove-glyph-from-array ((gamemap gamemap) (glyph glyph))
  (with-slots (gamemap-glyph-array) gamemap
    (let ((the-glyph (position glyph gamemap-glyph-array)))
      (when the-glyph
        (setf gamemap-glyph-array (delete the-glyph gamemap-glyph-array))))))

(defmethod remove-glyph-from-mapcell ((gamemap gamemap) (glyph glyph))
  (with-slots (glyph-game-position) glyph
    (with-mapcell-at-yx gamemap glyph-game-position mapcell
      (let ((the-glyph (position glyph mapcell)))
        (when the-glyph
          (setf gamemap-glyph-array (delete the-glyph mapcell)))))))

(defmethod get-glyph-in-array ((gamemap gamemap))
  (gamemap-glyph-array gamemap))

(defmethod get-glyph-in-mapcell ((gamemap gamemap) (game-position vec2))
  ())

(defmethod get-mouse-game-position ((gamemap gamemap))
  ())

(in-package :dungeon/glyph)
(defmethod initialize-instance :after ((glyph glyph) &rest initarg)
  (format t "glyph has been created.~%")
  (with-slots (glyph-gamemap) glyph
    (if glyph-gamemap
        (dungeon/gamemap:add-glyph glyph-gamemap glyph)
        (vector-push-extend glyph *glyph-array*))))

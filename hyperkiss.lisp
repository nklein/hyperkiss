
(ql:quickload '(:trivial-main-thread :cl-glu :cl-glut :rational-extensions/math :alexandria))

(use-package :rational-extensions)
(shadowing-import (package-shadowing-symbols :rational-extensions/math))
(use-package :rational-extensions/math)

(defparameter +tolerance+ 1/10000000)

(deftype vec-element ()
  '(or real rational-extension))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun vec-element-p (e)
    (typep e 'vec-element))

  (defun all-vec-element-p (l)
    (every #'vec-element-p l)))

(deftype vec ()
  `(and list
        (satisfies all-vec-element-p)))

(defun vec (&rest elts)
  (check-type elts vec)
  (copy-seq elts))

(defun vdims (v)
  (check-type v vec)
  (length v))

(defun v. (a b)
  (reduce #'+ (mapcar #'* a b)))

(defun v+ (a b)
  (check-type a vec)
  (check-type b vec)
  (apply #'vec (mapcar #'+ a b)))

(defun v- (a b)
  (check-type a vec)
  (check-type b vec)
  (apply #'vec (mapcar #'- a b)))

(defun vs (s v)
  (check-type s vec-element)
  (check-type v vec)
  (apply #'vec (mapcar (lambda (x) (* s x)) v)))

(defun vn (v &optional (length 1))
  (check-type v vec)
  (vs (/ length (sqrt (v. v v))) v))

(defun v< (a b)
  (labels ((rec (a b was-less-p)
             (cond
               ((or (null a)
                    (null b))
                was-less-p)
               (t
                (destructuring-bind (a &rest as) a
                  (destructuring-bind (b &rest bs) b
                    (or (< a b)
                        (and (= a b)
                             (rec as bs (< a b))))))))))
    (rec a b nil)))

(defun vavg (pts)
  (vs (/ (length pts)) (reduce #'v+ pts)))

(defun rationalize-vec (v &optional (length 1))
  (flet ((approximate (x)
           (let ((FACTOR (* 120 1024)))
             (/ (truncate (* x FACTOR))
                FACTOR))))
    (let* ((rv (mapcar #'approximate v)))
      (apply #'vec (list* (* (sqrt (- (* length length)
                                      (reduce #'+ (rest rv) :key (lambda (x) (* x x)))))
                             (if (minusp (first rv))
                                 -1
                                 1))
                          (rest rv))))))

(defvar *windows* nil)

(defparameter *show-shadows* t)
(defparameter *randomize* t)

(defparameter *spheres* (list (vec 2 0 0 0 0)
                              (vec 1 (sqrt 3) 0 0 0)
                              (vec 1 (/ (sqrt 3) 3) (* 2/3 (sqrt 6)) 0 0)
                              (vec 1 (/ (sqrt 3) 3) (* 1/6 (sqrt 6)) (* 1/2 (sqrt 10)) 0)
                              (vec 1 (/ (sqrt 3) 3) (* 1/6 (sqrt 6)) (* 1/10 (sqrt 10)) (* 2/5 (sqrt 15)))))

(defparameter *selected* nil)
(defparameter *overlap-wins* nil)
(defparameter *overlappers* nil)

(defparameter *rotation* (list (vec 1 0 0 0 0)
                               (vec 0 1 0 0 0)
                               (vec 0 0 1 0 0)
                               (vec 0 0 0 1 0)
                               (vec 0 0 0 0 1)))

(defun make-identity-matrix (dimensions)
  (loop :for ii :below dimensions
        :collecting (apply #'vec
                           (loop :for jj :below dimensions
                                 :collecting (if (= ii jj)
                                                 1
                                                 0)))))

(defun make-permutation (dimensions axises)
  (remove-duplicates (append axises
                             (loop :for ii :below dimensions
                                   :collecting ii))
                     :from-end t))

(defun transpose (A)
  (loop :for ii :below (length (first A))
        :collecting (apply #'vec
                           (loop :for row :in A
                                 :collecting (elt row ii)))))

#+(or)
(transpose (list (vec 1 2 3)
                 (vec 4 5 6)))

(defun mul-mat-mat (A B)
  (loop :for ii :below (length A)
        :for rowA :in A
        :collecting (apply #'vec
                           (loop :for jj :below (length (first B))
                                 :collecting (reduce #'+
                                                     (loop :for kk :below (length B)
                                                           :for aa := (elt rowA kk)
                                                           :for bb := (elt (elt B kk) jj)
                                                           :collecting (* aa bb)))))))

#+(or)
(let ((A (list (vec 1 2 0)
               (vec 2 3 4)))
      (B (list (vec 7 1 1 1)
               (vec 1 0 2 1)
               (vec 0 1 1 0))))
  ;; [ 1 2 0 ] [ 7 1 1 1 ]   [  9  1  5  3 ]
  ;; [ 2 3 4 ] [ 1 0 2 1 ] = [ 17  6 12  5 ]
  ;;           [ 0 1 1 0 ]
  (mul-mat-mat A B))

(defun mul-mat-vec (mat vec)
  (apply #'vec (loop :for row :in mat
                     :collecting (reduce #'+
                                         (loop :for mm :in row
                                               :for xx :in vec
                                               :collecting (* mm xx))))))

(defclass kiss-window (glut:window)
  ((dimensions :accessor kiss-window-dimensions
               :initargs :dimensions)
   (axises :reader kiss-window-axises
           :initarg :axises)
   (permutation :accessor kiss-window-permutation)
   (scale :accessor kiss-window-scale)
   (orig-click :accessor kiss-window-orig-click)
   (orig-rotation :accessor kiss-window-orig-rotation)
   (sphere-display-list-id :accessor kiss-window-sphere-display-list-id))
  (:default-initargs :width 600
                     :height 600
                     :dimensions 5
                     :axises '(0 1 2)
                     :mode '(:double :rgb :depth)))

(defmethod initialize-instance :after ((win kiss-window)
                                       &key
                                         dimensions
                                       &allow-other-keys)
  (setf (kiss-window-dimensions win) dimensions
        (kiss-window-permutation win) (make-permutation dimensions
                                                        (kiss-window-axises win))))

(defmethod make-title ((win kiss-window))
  (setf (glut:title win) (format nil "~aHyperKiss: ~{~a~^, ~}"
                                 (if (find win *overlap-wins*)
                                     "*"
                                     "")
                                 (kiss-window-axises win))))

(defmethod glut:display-window :before ((win kiss-window))
  (make-title win)

  (gl:clear-color 0 0 0 0)

  (let ((mat-ambient '(0.5 0.5 0.5 1.0))
	(mat-specular '(1.0 1.0 1.0 1.0))
	(mat-shininess 80))
    (gl:material :front :ambient mat-ambient)
    (gl:material :front :specular mat-specular)
    (gl:material :front :shininess mat-shininess))

  (let ((light-position '(1.0 1.0 1.0 0.0))
        (model-ambient '(0.5 0.5 0.5 1.0)))
    (gl:light :light0 :position light-position)
    (gl:light-model :light-model-ambient model-ambient)
    (gl:enable :lighting)
    (gl:enable :light0))

  (gl:enable :depth-test)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)

  (let ((quadric-obj (glu:new-quadric))
        (lists (gl:gen-lists 1)))
    (setf (kiss-window-sphere-display-list-id win) (+ lists 0))
    (glu:quadric-draw-style quadric-obj :fill)
    (glu:quadric-normals quadric-obj :smooth)
    (gl:with-new-list ((kiss-window-sphere-display-list-id win) :compile)
      (glu:sphere quadric-obj 1.0 36 25))
    (glu:delete-quadric quadric-obj)))

(defmethod draw-sphere ((win kiss-window) center radius color)
  (gl:material :front :shininess 80)
  (gl:material :front :ambient color)
  (apply #'gl:color color)
  (gl:with-pushed-matrix
    (apply #'gl:translate (mapcar (lambda (x) (* 1.0d0 x)) center))
    (gl:scale radius radius radius)
    (gl:call-list (kiss-window-sphere-display-list-id win))))

(defun permute (p v)
  (apply #'vec
         (loop :for kk :in p
               :collecting (elt v kk))))

(defmethod new-sphere-location ((win kiss-window))
  (let ((axises (kiss-window-axises win)))
    (vs 2 (rationalize-vec (elt *rotation* (elt axises 2))))))

(defun collect-combinations (sequence length)
  (let ((all))
    (alexandria:map-combinations (lambda (combo)
                                   (push combo all))
                                 sequence
                                 :length length)
    all))

(defmethod overlapp (a b)
  (< 2 (v. a b)))

(defmethod touchp (a b)
  (= 2 (v. a b)))

(defun simplexp (spheres)
  (and spheres
       (= (length spheres) (length (first spheres)))
       (progn
         (alexandria:map-combinations (lambda (pair)
                                        (unless (apply #'touchp pair)
                                          (return-from simplexp nil)))
                                      spheres
                                      :length 2)
         t)))

(defun valid-kissing-arrangement-p (&optional (spheres *spheres*) (tolerance 0))
  (loop :for a :in spheres
        :do (loop :for b :in spheres
                  :for ok := (progn
                               (cond
                                 ((eql a b)
                                  (<= (abs (- 4 (v. a b))) tolerance))
                                 (t
                                  (<= (v. a b) 2))))
                  :unless ok
                    :do (return-from valid-kissing-arrangement-p (values nil a b (* 1.0d0 (v. a b))))))
  t)

(defmethod get-ray-at-window-point ((win kiss-window) x y)
  (let* ((scale (kiss-window-scale win))
         (axises (kiss-window-axises win))
         (u (* (- x (/ (glut:width win) 2)) scale 1.0))
         (v (* (- (/ (glut:height win) 2) y) scale 1.0)))
    (values (v+ (v+ (vs u (elt *rotation* (elt axises 0)))
                    (vs v (elt *rotation* (elt axises 1))))
                (vs 10 (elt *rotation* (elt axises 2))))
            (vs -1 (elt *rotation* (elt axises 2))))))

(defun ray-sphere-intersect (origin direction ss)
  (let ((oc (v- ss origin))
        (radius 1))
      (let ((a (v. direction direction))
            (h (v. direction oc))
            (c (- (v. oc oc)
                  (* radius radius))))
        (let ((d (- (* h h)
                    (* a c))))
          (unless (minusp d)
            (let ((sqrtd (sqrt d)))
              (min (/ (- h sqrtd) a) (/ (+ h sqrtd) a))))))))

(defun find-sphere-hit (origin direction &optional (spheres *spheres*))
  (let ((best)
        (best-dist))
    (loop :for ss :in spheres
          :for dd := (ray-sphere-intersect origin direction ss)
          :do (when (and dd
                         (or (null best)
                             (< dd best-dist)))
                (setf best ss
                      best-dist dd))
          :finally (return (values best best-dist)))))

(defmethod hit-at-point ((win kiss-window) x y)
  (multiple-value-bind (origin direction) (get-ray-at-window-point win x y)
    (find-sphere-hit origin direction)))

(defmethod draw-projected-sphere ((win kiss-window) sphere r2fn color)
  (let ((center (permute (kiss-window-permutation win)
                         (mul-mat-vec *rotation* sphere))))
    (destructuring-bind (x y z &rest rest) center
      (let ((r2 (- 1.0d0 (if rest
                             (apply r2fn (mapcar (lambda (x) (* x x)) rest))
                             0))))
        (when (plusp r2)
          (draw-sphere win
                       (list x y z)
                       (sqrt r2)
                       color))))))

(defmethod draw-spheres ((win kiss-window) r2fn normal-color
                         &optional
                           (selected-color normal-color)
                           (overlapped-color normal-color))
  (dolist (sphere *spheres*)
    (draw-projected-sphere win
                           sphere
                           r2fn
                           (cond
                             ((find sphere *overlappers*)
                              overlapped-color)
                             ((find sphere *selected*)
                              selected-color)
                             (t
                              normal-color)))))

(defmethod draw-shadows ((win kiss-window) color
                         &optional (overlapped-color color))
  (let ((new-sphere-location (new-sphere-location win))
        (color (copy-seq color)))
    (dolist (sphere *spheres*)
      (let ((amount (min (re-realify (* 1/2 (v. sphere new-sphere-location)))
                         1)))
        (when (plusp amount)
          (setf (elt color 3) (max 0
                                   (* 0.75 amount)))
          (draw-projected-sphere win
                                 sphere
                                 (constantly (* 1/4 amount))
                                 (if (find sphere *overlappers*)
                                     overlapped-color
                                     color)))))))

(defun find-overlappers (sphere)
  (remove-if-not (lambda (s)
                   (overlapp s sphere))
                 *spheres*))

(defmethod find-overlappers-for-win ((win kiss-window))
  (find-overlappers (new-sphere-location win)))

(defun recalculate-overlappers ()
  (let ((orig (length *overlappers*)))
    (prog1
        (setf *overlappers* (apply #'append (mapcar #'find-overlappers-for-win *overlap-wins*)))
      (when (/= orig (length *overlappers*))
        (format *debug-io* "OVERLAPPERS = ~A => ~A~%" orig (length *overlappers*))))))

(defmethod glut:display ((win kiss-window))
  (let ((validp (valid-kissing-arrangement-p (list* (new-sphere-location win) *spheres*) +tolerance+)))
    (gl:clear :color-buffer :depth-buffer-bit)
    (draw-spheres win #'+ '(0.25 0.25 0.75 1.0) '(0.25 1.0 1.0 1.0) '(1.0 0.25 0.25 1.0))

    (gl:light-model :light-model-color-control :single-color)
    (gl:with-pushed-attrib (:lighting)
      (gl:disable :lighting)
      (when validp
        (draw-sphere win '(0.0 0.0 0.0) (re-realify (1- (sqrt 3))) '(1.0 0.5 0.0 0.5)))
      (draw-sphere win '(0.0 0.0 0.0) 1.0 '(1.0 0.5 0.0 0.5)))
    (when *show-shadows*
      (gl:with-pushed-attrib (:lighting)
        (gl:disable :lighting)
        (draw-shadows win '(0.75 0.25 0.75 0.5) '(1.0 0.25 0.25 0.75)))))
  (glut:swap-buffers))

(defun to-radians (deg)
  (* pi deg 1/180))

(defun post-redisplay-to-all-windows ()
  (recalculate-overlappers)
  (dolist (win *windows*)
    (glut:with-window win
      (glut:post-redisplay))))

(defmethod rotate-win ((win kiss-window) angle axis1 axis2)
  (with-accessors ((cos realpart) (sin imagpart)) (cis angle)
    (let ((axises (kiss-window-axises win)))
      (let ((axis1 (elt axises axis1))
            (axis2 (elt axises axis2)))
        (let ((a1 (elt *rotation* axis1))
              (a2 (elt *rotation* axis2)))
          (setf (elt *rotation* axis1) (v+ (vs cos a1)
                                           (vs (- sin) a2))
                (elt *rotation* axis2) (v+ (vs sin a1)
                                           (vs cos a2)))))))
  (post-redisplay-to-all-windows))

(defconstant +view-width+ 6.0)

(defmethod glut:reshape ((win kiss-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (setf (glut:width win) width
        (glut:height win) height)
  (let ((left (/ +view-width+ -2))
        (right (/ +view-width+ 2))
        (bottom (/ +view-width+ -2))
        (top (/ +view-width+ 2)))
    (if (<= width height)
        (setf bottom (/ (* bottom height) width)
              top    (/ (* top height) width)
              (kiss-window-scale win) (/ +view-width+ width))
        (setf left (/ (* left width) height)
              right (/ (* right width) height)
              (kiss-window-scale win) (/ +view-width+ height)))
    (gl:ortho left right bottom top -10.0 10.0))
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun reflect (x pts)
  (when pts
    (let ((mid (vavg pts)))
      (v+ x
          (vs 2
              (v- mid x))))))

(defun add-if-valid (s)
  (when (valid-kissing-arrangement-p (list* s *spheres*))
    (push s *spheres*)))

(defmethod add-reflected ((win kiss-window) hit selected)
  (let* ((reflected (vn (reflect hit (list* (apply #'vec (loop :repeat (kiss-window-dimensions win)
                                                               :collecting 0))
                                            (remove hit selected)))
                        2))
         (validp t))
    (cond
      ((and reflected
            validp
            (valid-kissing-arrangement-p (list* reflected *spheres*) +tolerance+))

       (pushnew reflected *spheres*)
       (post-redisplay-to-all-windows)
       t)
      ((and reflected
            validp)
       (format *debug-io* "THAT WOULD OVERLAP EXISTING SPHERES~%")
       nil)
      (t
       (format *debug-io* "NOT A VALID REFLECTION~%")
       nil))))

(defmethod glut:keyboard ((win kiss-window) key x y)
  (case key
    (#\1
     (setf *selected* (when *spheres*
                        (list (first *spheres*))))
     (post-redisplay-to-all-windows))
    (#\a
     (setf *selected* nil)
     (post-redisplay-to-all-windows))
    (#\i
     (setf *rotation* (make-identity-matrix (kiss-window-dimensions win)))
     (post-redisplay-to-all-windows))
    (#\f
     (let ((done nil))
       (flet ((try-to-fill (spheres)
                (when (simplexp spheres)
                  (dolist (hit (if *randomize*
                                   (alexandria:shuffle spheres)
                                   spheres))
                    (unless done
                      (with-output-to-string (*debug-io*)
                        (setf done (add-reflected win hit spheres))))))))
         (let ((all (collect-combinations *spheres* (kiss-window-dimensions win))))
           (map 'nil #'try-to-fill (if *randomize*
                                       (alexandria:shuffle all)
                                       all)))
         (when done
           (format *debug-io* "SPHERES = ~A~%" (length *spheres*))
           (post-redisplay-to-all-windows)))))
    (#\o
     (if (find win *overlap-wins*)
         (setf *overlap-wins* (remove win *overlap-wins*))
         (push win *overlap-wins*))
     (make-title win)
     (post-redisplay-to-all-windows))
    (#\s
     (setf *show-shadows* (not *show-shadows*))
     (post-redisplay-to-all-windows))
    (#\x
     (rotate-win win (to-radians 15) 1 2))
    (#\X
     (rotate-win win (to-radians -15) 1 2))
    (#\y
     (rotate-win win (to-radians 15) 2 0))
    (#\Y
     (rotate-win win (to-radians -15) 2 0))
    (#\z
     (rotate-win win (to-radians 15) 0 1))
    (#\Z
     (rotate-win win (to-radians -15) 0 1))
    (#\t
     (let ((hit (hit-at-point win x y)))
       (when hit
         (setf *selected* (remove-if (lambda (s)
                                       (not (or (touchp s hit)
                                                (overlapp s hit))))
                                     *spheres*))
         (format *debug-io* "SELECTED: ~A of ~A~%" (length *selected*) (length *spheres*))
         (post-redisplay-to-all-windows))))
    (#\T
     (let ((hit (hit-at-point win x y)))
       (when hit
         (setf *selected* (remove-if (lambda (s)
                                       (<= (v. s hit) 2))
                                     *spheres*))
         (format *debug-io* "SELECTED: ~A of ~A~%" (length *selected*) (length *spheres*))
         (post-redisplay-to-all-windows))))
    (#\D
     (let ((hit (hit-at-point win x y)))
       (when hit
         (setf *spheres* (remove hit *spheres*)
               *selected* (remove hit *selected*)
               *overlappers* (remove hit *overlappers*))
         (post-redisplay-to-all-windows))))
    (#\r
     (let ((hit (hit-at-point win x y)))
       (when hit
         (add-reflected win hit *selected*))))
    (#\Space
     (let ((hit (hit-at-point win x y)))
       (when hit
         (if (find hit *selected*)
             (setf *selected* (remove hit *selected*))
             (pushnew hit *selected*))
         (format *debug-io* "SELECTED: ~A of ~A~%" (length *selected*) (length *spheres*))
         (post-redisplay-to-all-windows))))
    (#\S
     (unless (add-if-valid (new-sphere-location win))
       (format *debug-io* "DOESN'T ACTUALLY FIT"))
     (post-redisplay-to-all-windows))
    (#\Esc
     (glut:destroy-current-window)))
  (glut:post-redisplay))

(defmethod glut:mouse ((win kiss-window) button state x y)
  (when (eq button :left-button)
    (setf (kiss-window-orig-click win) (when (eq state :down)
                                         (list x y))
          (kiss-window-orig-rotation win) (copy-seq *rotation*))))

(defmethod glut:motion ((win kiss-window) x y)
  (let ((orig-click (kiss-window-orig-click win)))
    (when orig-click
      (destructuring-bind (ox oy) orig-click
        (let ((dx (- x ox))
              (dy (- y oy)))
          (setf *rotation* (copy-seq (kiss-window-orig-rotation win)))
          (rotate-win win (to-radians dy) 1 2)
          (rotate-win win (to-radians (/ dx 3)) 2 0)))
      (post-redisplay-to-all-windows))))

(defmethod glut:close ((win kiss-window))
  (setf *windows* (remove win *windows*))
  (setf *overlap-wins* (remove win *overlap-wins*))
  (recalculate-overlappers)
  (post-redisplay-to-all-windows))

#+(or)
(trivial-main-thread:with-body-in-main-thread (:blocking nil)
  #+sbcl (sb-int:set-floating-point-modes :traps nil)
  (setf *spheres* (or (ignore-errors cl-user::*s*)
                      (list (vec 2 0 0 0 0)
                            (vec 1 (sqrt 3) 0 0 0)
                            (vec 1 (/ (sqrt 3) 3) (* 2/3 (sqrt 6)) 0 0)
                            (vec 1 (/ (sqrt 3) 3) (* 1/6 (sqrt 6)) (* 1/2 (sqrt 10)) 0)
                            (vec 1 (/ (sqrt 3) 3) (* 1/6 (sqrt 6)) (* 1/10 (sqrt 10)) (* 2/5 (sqrt 15))))))

  (setf *selected* nil)
  (setf *overlap-wins* nil)

  (setf *rotation* (list (vec 1 0 0 0 0)
                         (vec 0 1 0 0 0)
                         (vec 0 0 1 0 0)
                         (vec 0 0 0 1 0)
                         (vec 0 0 0 0 1)))
  (push (make-instance 'kiss-window :axises '(0 1 2)) *windows*)
  (push (make-instance 'kiss-window :axises '(1 2 3) :pos-x 610) *windows*)
  (push (make-instance 'kiss-window :axises '(2 3 4) :pos-x 1220) *windows*)
  (push (make-instance 'kiss-window :axises '(3 4 0) :pos-y 680 :height 400) *windows*)
  (push (make-instance 'kiss-window :axises '(4 0 1) :pos-x 610 :pos-y 680 :height 400) *windows*)
  (let ((cl-glut:*run-main-loop-after-display* nil))
    (mapcar #'glut:display-window *windows*))
  (glut:main-loop)
  *windows*)

#+(or)
(trivial-main-thread:with-body-in-main-thread (:blocking nil)
  #+sbcl (sb-int:set-floating-point-modes :traps nil)
  (setf *selected* nil)
  (setf *overlap-wins* nil)
  (setf *rotation* (make-identity-matrix 3))
  (setf *spheres* (list (vec 2 0 0)
                        (vec 1 (sqrt 3) 0)
                        (vec 1 (/ (sqrt 3) 3) (* 2/3 (sqrt 6)))))
  (push (make-instance 'kiss-window :axises '(0 1 2) :dimensions 3) *windows*)
  (let ((cl-glut:*run-main-loop-after-display* nil))
    (mapcar #'glut:display-window *windows*))
  (glut:main-loop)
  *windows*)

#+(or)
(trivial-main-thread:with-body-in-main-thread (:blocking nil)
  #+sbcl (sb-int:set-floating-point-modes :traps nil)
  (setf *selected* nil)
  (setf *overlap-wins* nil)
  (setf *rotation* (make-identity-matrix 4))
  (setf *spheres* (list (vec 2 0 0 0)
                        (vec 1 (sqrt 3) 0 0)
                        (vec 1 (/ (sqrt 3) 3) (* 2/3 (sqrt 6)) 0)
                        (vec 1 (/ (sqrt 3) 3) (* 1/6 (sqrt 6)) (* 1/2 (sqrt 10)))))
  (push (make-instance 'kiss-window :axises '(0 1 2) :dimensions 4) *windows*)
  (push (make-instance 'kiss-window :axises '(1 2 3) :dimensions 4 :pos-x 610) *windows*)
  (push (make-instance 'kiss-window :axises '(2 3 0) :dimensions 4 :pos-x 1220) *windows*)
  (push (make-instance 'kiss-window :axises '(3 0 1) :dimensions 4 :pos-y 610) *windows*)
  (let ((cl-glut:*run-main-loop-after-display* nil))
    (mapcar #'glut:display-window *windows*))
  (glut:main-loop)
  *windows*)

#+(or)
(setf *spheres* (rest *spheres*))

#+(or)
(let ((*a* (vec (* -1/60 (sqrt 2158))
                -19/60
                109/60
                0
                0))
      (*b* (vec 1
                (* 1/3 (sqrt 3))
                (* 2/3 (sqrt 6))
                0
                0))
      (*c* (- 2 (v. *a* *b*))))
  (format *debug-io* "~%~A =>%" *c*)
  *c*)

#+(or)
(setf *selected* (last *spheres* 5))


#+(or)
(let ((a (* 2/5 (sqrt 5))))
  (loop :for i :below 32
        :collecting (apply #'vec (loop :for j :below 5
                                       :for bits := i :then (ash bits -1)
                                       :collecting (if (zerop (logand bits 1))
                                                       a
                                                       (- a))))))


(defun guess (&optional (spheres *spheres*))
  (let ((mean (reduce #'v+ spheres)))
    (vs (/ -2 (sqrt (v. mean mean))) mean)))


(defun all-perms (&key (spheres *spheres*) (negativesp nil))
  (let ((all nil))
    (dolist (s spheres all)
      (alexandria:map-permutations (lambda (v)
                                     (pushnew v all :test #'equalp)
                                     (when negativesp
                                       (pushnew (vs -1 v) all :test #'equalp)))
                                   s))))


(defun maximal-cosine (&optional (spheres *spheres*))
  (let ((best nil)
        (best-combo nil))
    (flet ((place (vs)
             (destructuring-bind (a b) vs
               (let ((x (v. a b)))
                 (when (or (null best)
                           (< best x))
                   (setf best x
                         best-combo vs))))))
      (alexandria:map-combinations #'place spheres :length 2))
    (values best
            best-combo)))

#+(or)
(let ((p (all-perms :spheres (mapcar (lambda (v)
                                       (vn v 2))
                                     (list (vec 1 1 0 0 0)
                                           (vec 1 -1 0 0 0)
                                           (vec -1 -1 0 0 0)))
                    :negativesp nil)))
  (setf p (sort p #'v<))
  (values-list (list* p (length p) (multiple-value-list (valid-kissing-arrangement-p p)))))

(defun count-touchers (s &optional (spheres *spheres*))
  (count-if (lambda (a)
              (touchp s a))
            spheres))

#+(or)
(let ((best nil)
      (best-sphere nil))
  (dolist (a *spheres* (values best-sphere best))
    (let ((c (count-touchers a *spheres*)))
      (when (or (null best)
                (< c best))
        (setf best c
              best-sphere a)))))

#|
(-√2 -√2 0 0 0)
(-√2 0 -√2 0 0)
(-√2 0 0 -√2 0)
(-√2 0 0 0 -√2)
(-√2 0 0 0 √2)
(-√2 0 0 √2 0)
(-√2 0 √2 0 0)
(-√2 √2 0 0 0)
(0 -√2 -√2 0 0)
(0 -√2 0 -√2 0)
(0 -√2 0 0 -√2)
(0 -√2 0 0 √2)
(0 -√2 0 √2 0)
(0 -√2 √2 0 0)
(0 0 -√2 -√2 0)
(0 0 -√2 0 -√2)
(0 0 -√2 0 √2)
(0 0 -√2 √2 0)
(0 0 0 -√2 -√2)
(0 0 0 -√2 √2)
(0 0 0 √2 -√2)
(0 0 0 √2 √2)
(0 0 √2 -√2 0)
(0 0 √2 0 -√2)
(0 0 √2 0 √2)
(0 0 √2 √2 0)
(0 √2 -√2 0 0)
(0 √2 0 -√2 0)
(0 √2 0 0 -√2)
(0 √2 0 0 √2)
(0 √2 0 √2 0)
(0 √2 √2 0 0)
(√2 -√2 0 0 0)
(√2 0 -√2 0 0)
(√2 0 0 -√2 0)
(√2 0 0 0 -√2)
(√2 0 0 0 √2)
(√2 0 0 √2 0)
(√2 0 √2 0 0)
(√2 √2 0 0 0)
|#


#+(or)
(let ((x 0)
      (y (sqrt 5/8))
      (z (- (sqrt 2/5)))
      (w (- (sqrt 9/40))))
  (let ((a (vec 1 0 0 0 0))
        (b (vec 1/2 (sqrt 3/4) 0 0 0))
        (c (vec 0 (sqrt 1/3) (sqrt 2/3) 0 0))
        (d (vec 0 0 (sqrt 3/8) x y))
        (e (vec 0 0 (sqrt 3/8) z w)))
    (mapcar (lambda (s)
              (vn s 2))
            (list (vec (v. a a)
                       (v. a b)
                       (v. a c)
                       (v. a d)
                       (v. a e))
                  (vec (v. b a)
                       (v. b b)
                       (v. b c)
                       (v. b d)
                       (v. b e))
                  (vec (v. c a)
                       (v. c b)
                       (v. c c)
                       (v. c d)
                       (v. c e))
                  (vec (v. d a)
                       (v. d b)
                       (v. d c)
                       (v. d d)
                       (v. d e))
                  (vec (v. e a)
                       (v. e b)
                       (v. e c)
                       (v. e d)
                       (v. e e))))))

(defun v+list (v list)
  (mapcar (lambda (vp) (v+ v vp)) list))

(defun v-list (v list)
  (mapcar (lambda (vp) (v- vp v)) list))

(defun vslist (s list)
  (mapcar (lambda (vp) (vs s vp)) list))

(defun vnlist (s list)
  (mapcar (lambda (vp) (vn vp s)) list))

(defun mat+ (m1 m2)
  (loop :for a :in m1
        :for b :in m2
        :collecting (v+ a b)))

(defun mat- (m1 m2)
  (loop :for a :in m1
        :for b :in m2
        :collecting (v- a b)))

(defun make-abt (a b)
  (loop :for s :in a
        :collecting (vs s b)))

(defun make-yxt-xyt (x y)
  (let ((yxt (make-abt y x))
        (xyt (make-abt x y)))
    (mat- yxt xyt)))

(defun mats (s m)
  (loop :for row :in m
        :collecting (vs s row)))

(defun mat-that-takes-x-to-y (x y)
  (let* ((i (make-identity-matrix (vdims x)))
         (swizzle (make-yxt-xyt x y))
         (swizzle^2 (mul-mat-mat swizzle swizzle)))
    (mat+ (mat+ i
                swizzle)
          (mats (/ (1+ (v. x y)))
                swizzle^2))))

(defun simplex (dimensions)
  (let* ((m (vs (/ (1+ dimensions))
                (apply #'vec (loop :repeat (1+ dimensions) :collecting 1))))
         (x (vs (/ (sqrt (1+ dimensions)))
                (apply #'vec (loop :repeat (1+ dimensions) :collecting 1))))
         (y (apply #'vec (append (loop :repeat dimensions :collecting 0)
                                 (list 1))))
         (r (mat-that-takes-x-to-y x y))
         (shrinker (make-identity-matrix dimensions)))
    (loop :for i :below (1+ dimensions)
          :for v := (apply #'vec (loop :for j :below (1+ dimensions)
                                       :collecting (if (= i j) 1 0)))
          :collecting (vn (mul-mat-vec shrinker
                                       (mul-mat-vec r (v- v m))) 2))))

(defun mul-mat-veclist (mat list)
  (mapcar (lambda (v)
            (mul-mat-vec mat v))
          list))

(defun find-most-like (a list)
  (check-type a vec)
  (let* ((best (first list))
         (likeness (v. a best)))
    (dolist (v list (values best likeness))
      (check-type v vec)
      (let ((l (v. a v)))
        (when (< likeness l)
          (setf best v
                likeness l))))))

(defun rectify* (up list)
  (let ((most-up (find-most-like up list)))
    (mul-mat-veclist (mat-that-takes-x-to-y (vn most-up) up)
                     list)))

(defun rectify (list)
  (rectify* (apply #'vec (loop :for i :below (vdims (first list))
                               :collecting (if (zerop i) 1 0)))
            list))

(defun bloom-shape (offsets shape)
  (loop :for offset :in offsets
        :for noffset := (vn offset)
        :for rshape := (mul-mat-veclist (mat-that-takes-x-to-y (vn (first shape))
                                                               noffset)
                                        (rest shape))
        :for m := (vavg rshape)
        :for pt-m := (v- (first rshape) m)
        :for mscaled := (vn m
                            (sqrt (- 4 (v. pt-m pt-m))))
        :appending (v+list mscaled (v-list m rshape))))

(defun bloom-simplex (dimensions)
  (let ((simplex (simplex dimensions)))
    (loop :for i :below (length simplex)
          :for v := (nth i simplex)
          :for sublist := (append (subseq simplex 0 i)
                                  (subseq simplex (1+ i)))
          :for scaled := (vnlist 2 (v-list v sublist))
          :for m := (vavg scaled)
          :for pt-m := (v- (first scaled) m)
          :for mscaled := (vn m (sqrt (- 4 (v. pt-m pt-m))))
          :appending (v+list mscaled (v-list m scaled)))))

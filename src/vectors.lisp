(in-package :hyperkiss)

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

(defun vecp (v)
  (typep v 'vec))

(defun vec (&rest elts)
  (check-type elts vec)
  (copy-seq elts))

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
  (check-type length real)
  (vs (/ length (sqrt (v. v v))) v))

(defun v< (a b)
  (check-type a vec)
  (check-type b vec)
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
  (check-type pts list)
  (assert (every #'vecp list))
  (vs (/ (length pts)) (reduce #'v+ pts)))

(defun rationalize-vec (v &optional (length 1))
  (check-type v vec)
  (check-type length real)
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

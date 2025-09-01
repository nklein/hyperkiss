(in-package :hyperkiss)

(defun make-identity-matrix (dimensions)
  (loop :for ii :below dimensions
        :collecting (apply #'vec
                           (loop :for jj :below dimensions
                                 :collecting (if (= ii jj)
                                                 1
                                                 0)))))

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

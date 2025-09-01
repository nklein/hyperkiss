(in-package :hyperkiss)

(defun make-permutation (dimensions axises)
  (remove-duplicates (append axises
                             (loop :for ii :below dimensions
                                   :collecting ii))
                     :from-end t))

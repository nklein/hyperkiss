(defpackage :hyperkiss
  (:use #:cl #:rational-extensions)
  (:export #:vec
           #:vecp
           #:vec-element
           #:vec-element-p
           #:v.
           #:v+
           #:v-
           #:vs
           #:vn
           #:v<
           #:vavg
           #:rationalize-vec)
  (:export #:make-identity-matrix
           #:transpose
           #:mul-mat-mat
           #:mul-mat-vec)
  (:export #:make-permutation)
  (:export #:start-loop))

(shadowing-import (package-shadowing-symbols :rational-extensions/math))

;;;; hyperkiss.asd

(asdf:defsystem #:hyperkiss
  :description "HYPERKISS allows positioning of kissing spheres in three or more dimensions."
  :author "Patrick Stein <pat@nklein.com>"
  :license "UNLICENSE"
  :version "0.1.20250901"
  :depends-on (#:rational-extensions/math #:trivial-main-thread #:cl-glu #:cl-glut #:alexandria)
  :components
  ((:static-file "README.md")
   (:static-file "UNLICENSE.txt")
   (:module "src"
    :components ((:file "package")
                 (:file "vectors" :depends-on ("package"))
                 (:file "matrixes" :depends-on ("package"
                                                "vectors"))
                 (:file "permutations" :depends-on ("package"))))))

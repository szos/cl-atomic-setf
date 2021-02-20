;;;; cl-atomic-setf.asd

(asdf:defsystem #:cl-atomic-setf
  :description "Describe cl-atomic-setf here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-cltl2)
  :components ((:file "package")
               (:file "cl-atomic-setf")))

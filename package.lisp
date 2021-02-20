;;;; package.lisp

(defpackage #:cl-atomic-setf
  (:use #:cl)
  (:export #:with-atomic-setf
           #:with-atomic-setf-reset-error
           #:with-atomic-setf-reset-error-places
           #:with-atomic-setf-reset-warning
           #:with-atomic-setf-reset-warning-places))

;;;; cl-atomic-setf.lisp

(in-package #:cl-atomic-setf)

(defmacro with-gensyms (syms &body body)
  `(let (,@(mapcar (lambda (s)
                     (list s (list 'gensym (symbol-name s))))
                   syms))
     ,@body))

(define-condition atomic-reset-error (error) ())

(define-condition with-atomic-setf-reset-error (error)
  ((places :initarg :places
           :reader with-atomic-setf-reset-error-places
           :documentation "List of places retaining their original value")))

(define-condition with-atomic-setf-reset-warning (warning)
  ((places :initarg :places
           :reader with-atomic-setf-reset-warning-places
           :documentation "List of places retaining their original value")))

(defmacro %atomic-setf-reset (errorp accumulator &key (pop t))
  (declare (special *accumulator*))
  (let ((place-list (if pop
                        (cdr *accumulator*)
                        *accumulator*)))
    `(progn
       ,@(when pop `((pop ,accumulator)))
       (setf ,accumulator (reverse ,accumulator))
       ,@(loop for place in (reverse place-list)
               collect `(when (> (length ,accumulator) 0)
                          (setf ,place (pop ,accumulator))))
       ,(if errorp
            `(error 'with-atomic-setf-reset-error :places ',place-list)
            `(warn 'with-atomic-setf-reset-warning :places ',place-list)))))

(defmacro %atomic-setf-individual (conditions accumulator place value)
  (declare (special *accumulator*))
  (push place *accumulator*)
  `(progn (push ,place ,accumulator)
          (handler-case (setf ,place ,value)
            (,conditions ()
              (error 'atomic-reset-error)))))

(defmacro %atomic-setf (conditions accumulator &rest args)
  "collect individually wrapped setf statements (to catch errors within )"
  (declare (special *accumulator*))
  `(progn ,@(loop for (p v) on args by 'cddr
                  collect `(%atomic-setf-individual ,conditions ,accumulator ,p ,v))))

(defmacro with-atomic-setf ((&key errorp (handle-conditions 'error)) &body forms)
  (with-gensyms (accum args)
    `(trivial-cltl2:compiler-let ((*accumulator* nil))
       (let ((,accum '()))
         (handler-case
             (macrolet ((asetf (&body ,args)
                          `(%atomic-setf ,',handle-conditions
                                         ,',accum
                                         ,@,args))
                        (atomic-setf (&body ,args)
                          `(%atomic-setf ,',handle-conditions
                                         ,',accum
                                         ,@,args)))
               ,@forms)
           ((or ,handle-conditions atomic-reset-error) ()
             (%atomic-setf-reset ,errorp ,accum :pop nil)))))))

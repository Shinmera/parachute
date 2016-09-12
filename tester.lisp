#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

(defmacro define-tester (name args &body body))

(define-tester true (form)
  (emit-tests (funcall form)))

(define-tester false (form)
  (emit-tests (not (funcall form))))

(define-tester fail (form &optional (condition 'error))
  (emit-tests
   (handler-case
       (progn (funcall form) NIL)
     (error (err)
       (typep err condition)))))

(define-tester of-type (type form)
  (emit-tests
   (typep (funcall form) type)))

(define-tester is (comparator expected form)
  (emit-tests
   (funcall comparator expected (funcall form))))

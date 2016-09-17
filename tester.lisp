#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

(defun geq (value expected)
  (if expected
      value
      (not value)))

(defmacro capture-error (form &optional (condition 'error))
  (let ((err (gensym "ERR")))
    `(handler-case
         (prog1 NIL ,form)
       (,condition (,err)
         ,err))))

(defmacro true (form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'comparison-result
                   :expression ',form
                   :value (lambda () ,form)
                   :expected 'T
                   :comparison 'geq
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

(defmacro false (form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'comparison-result
                   :expression ',form
                   :value (lambda () ,form)
                   :expected 'NIL
                   :comparison 'geq
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

(defmacro is (comp expected form &optional description &rest format-args)
  (let ((exp (gensym "EXP")))
    `(let ((,exp ,expected))
       (eval-in-context
        *context*
        (make-instance 'comparison-result
                       :expression ',form
                       :value (lambda () ,form)
                       :expected ,exp
                       :comparison ',comp
                       ,@(when description
                           `(:description (format NIL ,description ,@format-args))))))))

(defmacro fail (form &optional (type 'error) description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'comparison-result
                   :expression '(capture-error ,form)
                   :value (lambda () (capture-error ,form ,type))
                   :expected ',type
                   :comparison 'typep
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

(defmacro of-type (type form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'comparison-result
                   :expression ',form
                   :value (lambda () ,form)
                   :expected ',type
                   :comparison 'typep
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

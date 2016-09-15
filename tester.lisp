#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

(defmacro true (form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'comparison-result
                   :expression ',form
                   :result (lambda () ,form)
                   :expected '(not null)
                   :comparison 'typep
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

(defmacro false (form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'comparison-result
                   :expression ',form
                   :result (lambda () ,form)
                   :expected 'null
                   :comparison 'typep
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

(defmacro is (comp form expected &optional description &rest format-args)
  (let ((exp (gensym "EXP")))
    `(let ((,exp ,expected))
       (eval-in-context
        *context*
        (make-instance 'comparison-result
                       :expression ',form
                       :result (lambda () ,form)
                       :expected ,exp
                       :comparison ',comp
                       ,@(when description
                           `(:description (format NIL ,description ,@format-args))))))))

(defmacro fail (form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'comparison-result
                   :expression ',form
                   :result (lambda () (nth-value 1 (ignore-errors ,form)))
                   :expected 'error
                   :comparison 'typep
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

(defmacro of-type (type form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'comparison-result
                   :expression ',form
                   :result (lambda () ,form)
                   :expected ',type
                   :comparison 'typep
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

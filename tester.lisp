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
                   :expression '(true ,form)
                   :value-form ',form
                   :value (lambda () ,form)
                   :expected 'T
                   :comparison 'geq
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

(defmacro false (form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'comparison-result
                   :expression '(false ,form)
                   :value-form ',form
                   :value (lambda () ,form)
                   :expected 'NIL
                   :comparison 'geq
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

(defmacro is (comp expected form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'comparison-result
                   :expression '(is ,comp ,expected ,form)
                   :value-form ',form
                   :value (lambda () ,form)
                   :expected ,expected
                   :comparison ,(maybe-quote comp)
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

(defmacro isnt (comp expected form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'comparison-result
                   :expression '(is ,comp ,expected ,form)
                   :value-form ',form
                   :value (lambda () ,form)
                   :expected ,expected
                   :comparison ,(maybe-quote comp)
                   :comparison-geq NIL
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

(defmacro fail (form &optional (type 'error) description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'comparison-result
                   :expression '(fail ,form ,type)
                   :value-form '(capture-error ,form)
                   :value (lambda () (capture-error ,form ,(maybe-unquote type)))
                   :expected ',(maybe-unquote type)
                   :comparison 'typep
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

(defmacro of-type (type form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'comparison-result
                   :expression '(of-type ,type ,form)
                   :value-form ',form
                   :value (lambda () ,form)
                   :expected ',(maybe-unquote type)
                   :comparison 'typep
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

(defmacro finish (form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'finishing-result
                   :expression '(finish ,form)
                   :value-form '(complete ,form)
                   :value (lambda () ,form)
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

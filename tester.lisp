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
                   :body (lambda () ,form)
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
                   :body (lambda () ,form)
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
                   :body (lambda () ,form)
                   :expected ,expected
                   :comparison ,(maybe-quote comp)
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

(defmacro isnt (comp expected form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'comparison-result
                   :expression '(isnt ,comp ,expected ,form)
                   :value-form ',form
                   :body (lambda () ,form)
                   :expected ,expected
                   :comparison ,(maybe-quote comp)
                   :comparison-geq NIL
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

(defun destructure-is-values-body (body)
  (let ((expected ()) (comparison ()) (description NIL) (format-args ()))
    (values (loop for (expr . rest) on body
                  do (when (stringp expr)
                       (setf description expr)
                       (setf format-args rest)
                       (loop-finish))
                     (push (first expr) comparison)
                     (push (second expr) expected)
                  collect expr)
            (nreverse expected)
            (mapcar #'maybe-quote (nreverse comparison))
            description format-args)))

(defmacro is-values (form &body body)
  (multiple-value-bind (comp-expected expected comparison description format-args)
      (destructure-is-values-body body)
    `(eval-in-context
      *context*
      (make-instance 'multiple-value-comparison-result
                     :expression '(is-values ,form ,@comp-expected)
                     :value-form ',form
                     :body (lambda () ,form)
                     :expected (list ,@expected)
                     :comparison (list ,@comparison)
                     ,@(when description
                         `(:description (format NIL ,description ,@format-args)))))))

(defmacro isnt-values (form &body body)
  (multiple-value-bind (comp-expected expected comparison description format-args)
      (destructure-is-values-body body)
    `(eval-in-context
      *context*
      (make-instance 'multiple-value-comparison-result
                     :expression '(isnt-values ,form ,@comp-expected)
                     :value-form ',form
                     :body (lambda () ,form)
                     :expected (list ,@expected)
                     :comparison (list ,@comparison)
                     :comparison-geq NIL
                     ,@(when description
                         `(:description (format NIL ,description ,@format-args)))))))

(defmacro fail (form &optional (type 'error) description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'comparison-result
                   :expression '(fail ,form ,type)
                   :value-form '(capture-error ,form)
                   :body (lambda () (capture-error ,form ,(maybe-unquote type)))
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
                   :body (lambda () ,form)
                   :expected ',(maybe-unquote type)
                   :comparison 'typep
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

(defmacro finish (form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'finishing-result
                   :expression '(finish ,form)
                   :body (lambda () ,form)
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

(defmacro with-forced-status ((status &optional description &rest format-args) &body tests)
  `(eval-in-context
    *context*
    (make-instance 'controlling-result
                   :expression ,status
                   :child-status ,status
                   :body (lambda () ,@tests)
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

(defmacro skip (desc &body tests)
  `(with-forced-status (:skipped ,@(if (listp desc) desc (list desc)))
     ,@tests))

(defmacro skip-on (features desc &body tests)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk ()
              ,@tests))
       (if (featurep '(or ,@features))
           (with-forced-status (:skipped ,@(if (listp desc) desc (list desc)))
             (,thunk))
           (,thunk)))))

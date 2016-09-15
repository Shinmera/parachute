#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

(defmacro true (form &optional description &rest format-args)
  `(eval-in-context
    (make-instance 'comprison-result
                   :expression ',form
                   :expected '(not null)
                   :comparison 'typep
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))
    (lambda () ,form)))

(defmacro false (form &optional description &rest format-args)
  `(eval-in-context
    (make-instance 'comprison-result
                   :expression ',form
                   :expected 'null
                   :comparison 'typep
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))
    (lambda () ,form)))

(defmacro is (comp expected form &optional description &rest format-args)
  (let ((exp (gensym "EXP")))
    `(let ((,exp ,expected))
       (eval-in-context
        (make-instance 'comprison-result
                       :expression ',form
                       :expected ,exp
                       :comparison ',comp
                       ,@(when description
                           `(:description (format NIL ,description ,@format-args))))
        (lambda () ,form)))))

(defmacro fail (form &optional description &rest format-args)
  `(eval-in-context
    (make-instance 'comprison-result
                   :expression ',form
                   :expected 'error
                   :comparison 'typep
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))
    (lambda () (nth-value 1 (ignore-errors ,form)))))

(defmacro of-type (type form &optional description &rest format-args)
  `(eval-in-context
    (make-instance 'comprison-result
                   :expression ',form
                   :expected ',type
                   :comparison 'typep
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))
    (lambda () ,form)))

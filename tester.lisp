#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

(defclass test-result ()
  ((expression :initarg :expression :accessor expression)
   (description :initarg :description :accessor description)))

(defclass comparison-result (test-result)
  ((result :initarg :result :accessor result)
   (expected :initarg :expected :accessor expected)
   (comparison :initarg :comparison :accessor comparison)))

(defclass subtest-result (test-result)
  ((children :initarg :children :accessor children)))

(defmacro true (form &optional description &rest format-args)
  `(make-instance 'comprison-result
                  :expression ',form
                  :result ,form
                  :expected '(not null)
                  :comparison 'typep
                  ,@(when description
                      `(:description (format NIL ,description ,@format-args)))))

(defmacro false (form &optional description &rest format-args)
  `(make-instance 'comprison-result
                  :expression ',form
                  :result ,form
                  :expected 'null
                  :comparison 'typep
                  ,@(when description
                      `(:description (format NIL ,description ,@format-args)))))

(defmacro is (comp expected form &optional description &rest format-args)
  (let ((exp (gensym "EXP")))
    `(let ((,exp ,expected))
       (make-instance 'comprison-result
                      :expression ',form
                      :result ,form
                      :expected ,exp
                      :comparison ',comp
                      ,@(when description
                          `(:description (format NIL ,description ,@format-args)))))))

(defmacro fail (form &optional description &rest format-args)
  `(make-instance 'comprison-result
                  :expression ',form
                  :result (nth-value 1 (ignore-errors ,form))
                  :expected 'error
                  :comparison 'typep
                  ,@(when description
                      `(:description (format NIL ,description ,@format-args)))))

(defmacro of-type (type form &optional description &rest format-args)
  `(make-instance 'comprison-result
                  :expression ',form
                  :result (type-of ,form)
                  :expected ',type
                  :comparison 'typep
                  ,@(when description
                      `(:description (format NIL ,description ,@format-args)))))

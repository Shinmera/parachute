#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

(defvar *context* NIL)

(defmethod result-for-testable (whatever (test test))
  (make-instance 'parent-result :expression test))

(defmethod eval-in-context (context (func function))
  (let ((*context* context))
    (funcall func)))

(defmethod eval-in-context ((null null) test)
  (let ((context (result-for-testable null test)))
    (eval-in-context context test)
    context))

(defclass result ()
  ((expression :initarg :expression :accessor expression)
   (status :initarg :status :accessor status)
   (duration :initarg :duration :accessor duration)
   (description :initarg :description :accessor description))
  (:default-initargs
   :expression (error "EXPRESSION required.")
   :status :unknown
   :duration NIL
   :description NIL))

(defmethod print-object ((result result) stream)
  (print-unreadable-object (result stream :type T)
    (format stream "~s ~a" (status result) (expression result))))

(defmethod eval-in-context :around ((result result) thing)
  (let ((start (get-internal-real-time)))
    (unwind-protect
         (call-next-method)
      (setf (duration result) (/ (- (get-internal-real-time) start)
                                 internal-time-units-per-second)))))

(defmethod eval-in-context :after ((context result) thing)
  (when (and *context*
             (not (eql context *context*))
             (not (eql thing *context*)))
    (eval-in-context *context* context))
  ;; Mark ourselves as passed if we didn't already set the status.
  (when (eql :unknown (status context))
    (setf (status context) :passed)))

(defclass comparison-result (result)
  ((result :initarg :result :accessor result)
   (expected :initarg :expected :accessor expected)
   (comparison :initarg :comparison :accessor comparison))
  (:default-initargs
   :expected '(not null)
   :comparison 'typep))

(defmethod print-object ((result comparison-result) stream)
  (print-unreadable-object (result stream :type T)
    (format stream "~s (~a ~s ~s)"
            (status result)
            (comparison result)
            (expression result)
            (expected result))))

(defmethod eval-in-context ((context comparison-result) thing)
  (setf (result context) (call-next-method))
  (if (ignore-errors (funcall (comparison context)
                              (result context)
                              (expected context)))
      (setf (status context) :passed)
      (setf (status context) :failed)))

(defclass parent-result (result)
  ((children :initarg :children :accessor children))
  (:default-initargs
   :children NIL))

(defmethod find-child-result (test (result parent-result))
  (find test (children result) :key #'expression :test #'eq))

(defmethod result-for-testable ((result parent-result) (test test))
  (or (find-child-result test result)
      (call-next-method)))

(defmethod eval-in-context ((context parent-result) (result result))
  (pushnew result (children context))
  (when (eql :failed (status result))
    (setf (status context) :failed)))

(defmethod eval-in-context :around ((context parent-result) (test test))
  (or (find-child-result test context)
      (progn
        ;; We have to run the dependencies here as they need to run before
        ;; the timing grips in the AROUND method of the RESULT class for
        ;; EVAL-IN-CONTEXT, which would count them running in a BEFORE.
        (dolist (dep (dependencies test))
          (eval-in-context context dep))
        (call-next-method))))

(defmethod eval-in-context ((context parent-result) (test test))
  (let ((result (result-for-testable context test)))
    (eval-in-context context result)
    (cond ((loop for dep in (dependencies test)
                 for result = (find-child-result context dep)
                 thereis (eql :failed (status result)))
           (setf (status result) :skipped))
          (T
           (loop for test in (tests test)
                 do (eval-in-context result test))))))

(defmethod eval-in-context :after ((context parent-result) (test test))
  (let ((skipped (skipped-children test)))
    (loop for child in (children test)
          do (cond ((find child skipped)
                    (let ((result (result-for-testable context child)))
                      (setf (status result) :skipped)
                      (eval-in-context context result)))
                   (T
                    (eval-in-context context child))))))

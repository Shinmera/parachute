#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

(defvar *parent* NIL)
(defvar *context* NIL)

(defmethod eval-in-context :around (context thing)
  (let ((*context* context))
    (call-next-method)))

(defmethod result-for-testable ((test test) context)
  (make-instance 'test-result :expression test))

(defmethod eval-in-context (context (test test))
  (eval-in-context context (result-for-testable test context)))

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

(defmethod initialize-instance :after ((result result) &key)
  (when *parent* ;; I wish I had a better place for this...
    (push result (children *parent*)))
  (when *context*
    (pushnew result (children *context*))))

(defmethod print-object ((result result) stream)
  (print-unreadable-object (result stream :type T)
    (format stream "~s ~a" (status result) (expression result))))

(defmethod print-object ((result result) (type symbol))
  (format NIL "~a" (expression result)))

(defmethod eval-in-context :around (context (result result))
  ;; Unless the status is unknown marked we should probably skip.
  (when (eql :unknown (status result))
    (let ((start (get-internal-real-time)))
      (unwind-protect
           (call-next-method)
        (setf (duration result) (/ (- (get-internal-real-time) start)
                                   internal-time-units-per-second))))
    ;; Mark ourselves as passed if we didn't already set the status.    
    (when (eql :unknown (status result))
      (setf (status result) :passed))))

(defclass comparison-result (result)
  ((result :initarg :result :accessor result)
   (expected :initarg :expected :accessor expected)
   (comparison :initarg :comparison :accessor comparison))
  (:default-initargs
   :expected '(not null)
   :comparison 'typep))

(defmethod print-object ((result comparison-result) stream)
  (print-unreadable-object (result stream :type T)
    (format stream "~s ~a"
            (status result)
            (print-object result :oneline))))

(defmethod print-object ((result comparison-result) (type (eql :oneline)))
  (format NIL "(~a ~s ~s)"
          (comparison result) (expression result) (expected result)))

(defmethod print-object ((result comparison-result) (type (eql :extensive)))
  (format NIL "The test form   ~a~%~
               evaluated to    ~a~%~
               when            ~a~%~
               was expected under ~a."
          (expression result) (result result) (expected result) (comparison result)))

(defmethod eval-in-context (context (result comparison-result))
  (setf (result result) (typecase (result result)
                           (function (funcall (result result)))
                           (T (result result))))
  (if (ignore-errors (funcall (comparison result)
                              (result result)
                              (expected result)))
      (setf (status result) :passed)
      (setf (status result) :failed)))

(defclass parent-result (result)
  ((children :initarg :children :accessor children))
  (:default-initargs
   :children NIL))

(defmethod result-for-testable ((test test) (result parent-result))
  (or (find test (children result) :key #'expression)
      (call-next-method)))

(defmethod eval-in-context :around (context (result parent-result))
  (let ((*parent* result))
    (call-next-method)))

(defmethod eval-in-context :after (context (result parent-result))
  (when (loop for child in (children result)
              thereis (eql :failed (status child)))
    (setf (status result) :failed)))

(defmethod find-child-result (test (result parent-result))
  (find test (children result) :key #'expression :test #'eq))

(defmethod results-with-status (status (result parent-result))
  (loop for result in (children result)
        when (eql status (status result))
        collect result))

(defclass test-result (parent-result)
  ())

(defmethod print-object ((result test-result) (type (eql :oneline)))
  (format NIL "~a::~a"
          (package-name (home (expression result)))
          (name (expression result))))

(defmethod eval-in-context :around (context (result test-result))
  ;; We have to run the dependencies here as they need to run before
  ;; the timing grips in the AROUND method of the RESULT class for
  ;; EVAL-IN-CONTEXT, which would count them running in a BEFORE.
  (dolist (dep (dependencies (expression result)))
    (eval-in-context context dep))
  (call-next-method))

(defmethod eval-in-context (context (result test-result))
  (let* ((test (expression result))
         (result (result-for-testable test context)))
    (cond ((loop for dep in (dependencies test)
                 for result = (find-child-result dep context)
                 thereis (eql :failed (status result)))
           (setf (status result) :skipped))
          (T
           (loop for test in (tests test)
                 do (funcall test))))))

(defmethod eval-in-context :after (context (result test-result))
  (let* ((test (expression result))
         (skipped (skipped-children test)))
    (loop for child in (children test)
          for subresult = (result-for-testable child context)
          do (cond ((find child skipped)
                    (setf (status child) :skipped)
                    (eval-in-context context subresult))
                   (T
                    (eval-in-context context subresult))))))

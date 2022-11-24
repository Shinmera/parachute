#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

(defun resolve-tests (designator)
  (etypecase designator
    (test (list designator))
    (list (mapcan #'resolve-tests designator))
    (package (package-tests designator))
    ((or symbol string)
     (cond ((find-test designator)
            (list (find-test designator)))
           ((find-package designator)
            (package-tests designator))
           (T
            (error "No test or package found for ~s." designator))))))

(defun test (designator &rest args &key (report 'plain) &allow-other-keys)
  (let* ((tests (resolve-tests designator))
         (report (apply #'make-instance report
                        :expression designator
                        (removef args :report)))
         (*context* report))
    (loop for test in tests
          for result = (result-for-testable test report)
          do (eval-in-context report result)
             (when (eql :failed (status result))
               (setf (status report) :failed)))
    (when (eql :unknown (status report))
      (setf (status report) :passed))
    (prog1 (summarize report)
      (when (and (boundp 'cl-user::*exit-on-test-failures*)
                 (symbol-value 'cl-user::*exit-on-test-failures*))
        (uiop:quit (if (eql :failed (status report)) 100 0))))))

(defun test-toplevel (designator/s &rest args)
  (let ((failure NIL))
    (loop for test in (if (listp designator/s) designator/s (list designator/s))
          for report = (apply #'test test args)
          do (when (eql :failed (status report))
               (setf failure T)))
    (uiop:quit (if failure 100 0))))

(defclass report (parent-result)
  ())

(defmethod print-object ((report report) stream)
  (print-unreadable-object (report stream :type T)
    (format stream "~s, ~a results"
            (length (results report))
            (status report))))

(defmethod tests-with-status (status (report report))
  (delete-if-not (lambda (a) (typep a 'test))
                 (mapcar #'expression (results-with-status status report))))

(defmethod summarize ((report report))
  report)

(defclass quiet (report)
  ())

(defmethod eval-in-context :around ((report quiet) (result result))
  (when (eql :unknown (status result))
    (handler-case
        (call-next-method)
      (error (err)
        (declare (ignore err))
        (setf (status result) :failed)))))

(defclass plain (report)
  ((output :initarg :output :initarg :stream :accessor output)
   (show-listing-p :initarg :show-listing :accessor show-listing-p))
  (:default-initargs :stream *standard-output*
                     :show-listing T))

(defclass summary (plain)
  ()
  (:default-initargs :show-listing nil))

(defvar *level* 0)

(defmethod eval-in-context :before ((report plain) (result parent-result))
  ;; This leads to more readable traces as otherwise the hierarchy is
  ;; printed with parents after children.
  (report-on result report))

(defmacro maybe-do-not-silence-errors (&body body)
  `(if *silence-plain-compilation-errors-p*
       ,@body
       (let ((ok nil))
         (unwind-protect
              (multiple-value-prog1
                  (call-next-method)
                (setf ok t))
           (unless ok (setf (status result) :failed))))))

(defmethod eval-in-context :around ((report plain) (result result))
  (when (eql :unknown (status result))
    (multiple-value-prog1
        (maybe-do-not-silence-errors
          (handler-case
              (call-next-method)
            (error (err)
              (warn "Unhandled error when evaluating ~a:~%  ~a~%"
                    (or (ignore-errors (format-result result :oneline))
                        result)
                    err)
              (setf (status result) :failed))))
      (report-on result report))))

(defmethod eval-in-context ((report plain) (result value-result))
  (maybe-do-not-silence-errors
    (handler-case
        (call-next-method)
      (error (err)
        (setf (value result) (if (typep result 'multiple-value-result) (list err) err))
        (setf (status result) :failed)))))

(defmethod eval-in-context ((report plain) (result finishing-result))
  (handler-case
      (call-next-method)
    (error ())))

(defmethod eval-in-context ((report plain) (result result))
  (let ((*level* (1+ *level*)))
    (call-next-method)))

(defmethod report-on :around (thing (report plain))
  (when (show-listing-p report)
    (call-next-method)))

(defmethod report-on :before ((result result) (report plain))
  (format (output report)
          "~& ~:[      ~;~:*~6,3f~] ~a~v@{  ~} "
          (duration result)
          (status-character (status result))
          *level* T))

(defmethod report-on :after (thing (report plain))
  (terpri (output report))
  (force-output (output report)))

(defmethod report-on ((result result) (report plain))
  (write-string (format-result result :oneline) (output report)))

(defun filter-test-results (results)
  (remove-if (lambda (a) (typep a 'test-result)) results))

(defmethod summarize ((report plain))
  (let ((failures (results-with-status :failed report)))
    (format (output report)
            "~&~%~
             ;; Summary:~%~
             Passed:  ~4d~%~
             Failed:  ~4d~%~
             Skipped: ~4d~%"
            (length (filter-test-results (results-with-status :passed report)))
            (length (filter-test-results failures))
            (length (results-with-status :skipped report)))
    (when failures
      (format (output report) "~&~%;; Failures:~%")
      (dolist (failure failures)
        (if (typep failure 'parent-result)
            (format (output report) "~&~a~%" (format-result failure :extensive))
            (format (output report) "~&~a~%~%" (format-result failure :extensive))))))
  report)

(defclass interactive (report)
  ())

(defmacro lformat (format &rest args)
  `'(lambda (s) (format s ,format ,@args)))

(defmethod eval-in-context :around ((report interactive) (result result))
  (restart-case
      (call-next-method)
    (retry ()
      :report (lambda (s) (format s "Retry testing ~a" (format-result result :oneline)))
      (eval-in-context report result))
    (abort ()
      :report (lambda (s) (format s "Continue, failing ~a" (format-result result :oneline)))
      (setf (status result) :failed))
    (continue ()
      :report (lambda (s) (format s "Continue, skipping ~a" (format-result result :oneline)))
      (setf (status result) :skipped))
    (pass ()
      :report (lambda (s) (format s "Continue, passing ~a" (format-result result :oneline)))
      (setf (status result) :passed))))

(defmethod eval-in-context :after ((report interactive) (result result))
  (when (eql :failed (status result))
    (error "Test ~a failed:~%~a"
           (print-oneline (expression result) NIL)
           (format-result result :extensive))))

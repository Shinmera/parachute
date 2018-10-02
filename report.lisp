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
    (dolist (test tests)
      (eval-in-context report (result-for-testable test report)))
    (summarize report)))

(defun test-toplevel (designator/s &rest args)
  (let ((failure NIL))
    (loop for test in (if (listp designator/s) designator/s (list designator/s))
          for report = (apply #'test test args)
          do (when (find :failed (results report) :key #'status)
               (setf failure T)))
    (uiop:quit (if failure 100 0))))

(defclass report (parent-result)
  ())

(defmethod print-object ((report report) stream)
  (print-unreadable-object (report stream :type T)
    (format stream "~a results"
            (length (results report)))))

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
  ((output :initarg :output :initarg :stream :accessor output))
  (:default-initargs :stream T))

(defvar *level* 0)

(defmethod eval-in-context :before ((report plain) (result parent-result))
  ;; This leads to more readable traces as otherwise the hierarchy is
  ;; printed with parents after children.
  (report-on result report))

(defmethod eval-in-context :around ((report plain) (result result))
  (when (eql :unknown (status result))
    (handler-case
        (call-next-method)
      (error (err)
        (warn "Unhandled error when evaluating ~a:~%  ~a~%" result err)
        (setf (status result) :failed)))
    (report-on result report)))

(defmethod eval-in-context ((report plain) (result value-result))
  (handler-case
      (call-next-method)
    (error (err)
      (setf (value result) err)
      (setf (status result) :failed))))

(defmethod eval-in-context ((report plain) (result result))
  (let ((*level* (1+ *level*)))
    (call-next-method)))

(defmethod report-on :before ((result result) (report plain))
  (format (output report)
          "~& ~:[      ~;~:*~6,3f~] ~a~v@{  ~} "
          (duration result)
          (case (status result)
            (:passed  #+asdf-unicode "✔" #-asdf-unicode "o")
            (:failed  #+asdf-unicode "✘" #-asdf-unicode "x")
            (:skipped #+asdf-unicode "ー" #-asdf-unicode "-")
            (T        #+asdf-unicode "？" #-asdf-unicode "?"))
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
      (format T "~&~%;; Failures:~%")
      (dolist (failure failures)
        (when (typep failure 'test-result)
          (format T "~&~a~%" (format-result failure :extensive))
          (dolist (failure (results-with-status :failed failure))
            (format T "~&~a~%~%" (format-result failure :extensive)))))))
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

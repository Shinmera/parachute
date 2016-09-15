#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

(defun resolve-test (name)
  (etypecase name
    (list (mapcan #'resolve-test name))
    (package (package-tests name))
    (symbol (cond ((find-test name)
                   (list (find-test name)))
                  ((find-package name)
                   (package-tests name))
                  (T
                   (error "No test or package found for ~s." name))))))

(defun test (name &rest args &key (report 'plain) &allow-other-keys)
  (let* ((tests (resolve-test name))
         (report (apply #'make-instance report :expression name args)))
    (dolist (test tests)
      (eval-in-context report test))
    (summarize report)))

(defclass report (parent-result)
  ())

(defmethod tests-with-status (status (report report))
  (delete-if-not (lambda (a) (typep a 'test))
                 (mapcan #'expression (results-with-status status report))))

(defclass plain (report)
  ())

(defvar *level* 0)

(defmethod eval-in-context :before ((report plain) (result test-result))
  ;; This leads to more readable traces as otherwise the hierarchy is
  ;; printed with parents after children.
  (report-on result report))

(defmethod eval-in-context :around ((report plain) (result result))
  (when (eql :unknown (status result))
    (call-next-method)
    (report-on result report)))

(defmethod eval-in-context ((report plain) (result result))
  (let ((*level* (1+ *level*)))
    (call-next-method)))

(defmethod report-on :before ((result result) (report plain))
  (format T "~& ~:[      ~;~:*~6,3f~] ~a~v@{    ~} "
          (duration result)
          (case (status result)
            (:passed  #+asdf-unicode "✔" #-asdf-unicode "o")
            (:failed  #+asdf-unicode "✘" #-asdf-unicode "x")
            (:skipped #+asdf-unicode "ー" #-asdf-unicode "-")
            (T        #+asdf-unicode "？" #-asdf-unicode "?"))
          *level* T))

(defmethod report-on :after (thing (report plain))
  (terpri)
  (force-output))

(defmethod report-on ((result result) (report plain))
  (write-string (print-object result :oneline)))

(defun filter-test-results (results)
  (remove-if (lambda (a) (typep a 'test-result)) results))

(defmethod summarize ((report plain))
  (let ((failures (results-with-status :failed report)))
    (format T "~&~%~
             ;; Summary:~%~
             Passed:  ~4d~%~
             Failed:  ~4d~%~
             Skipped: ~4d~%"
            (length (filter-test-results (results-with-status :passed report)))
            (length (filter-test-results failures))
            (length (filter-test-results (results-with-status :skipped report))))
    (when failures
      (format T "~&~%;; Failures:~%")
      (dolist (failure failures)
        (when (typep (expression failure) 'test)
          (let ((failures (results-with-status :failed failure)))
            (format T "~& ~4d/~4d tests failed in ~a:~%"
                    (length failures) (length (children failure))
                    (print-object failure :oneline))
            (dolist (failure failures)
              (format T "~&~a~%~%" (print-object failure :extensive))))))))
  report)

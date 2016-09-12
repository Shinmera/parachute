#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

(defvar *report*)

(defgeneric completed-results (report))
(defgeneric test-result (report test))
(defgeneric passed-tests (report))
(defgeneric failed-tests (report))
(defgeneric skipped-tests (report))
(defgeneric timing (report &optional test))
(defgeneric run (test-ish report))
(defgeneric report (result report))

(defclass result ()
  ((about :initarg :test :accessor about)
   (status :initarg :status :accessor status)
   (duration :initarg :duration :accessor duration))
  (:default-initargs
   :test (error "TEST required.")
   :status :running
   :duration NIL))

(defclass report ()
  ((about :initarg :test :accessor about)
   (results :initform NIL :accessor results))
  (:default-initargs
   :test (error "TEST required.")))

(defmethod completed-tests ((report report))
  (mapcar #'test (results report)))

(defmethod test-result ((report report) test)
  (find (find-test test) (results report) :key #'about))

(defmethod (setf test-result) (result (report report) test)
  (setf (results report) (list* result (remove test (results report) :key #'about))))

(defmethod test-status ((report report) test)
  (let ((result (test-result report test)))
    (when result (status result))))

(defun tests-with-status (status report)
  (loop for result in (results report)
        when (eql status (status result))
        collect (test result)))

(defmethod passed-tests ((report report))
  (tests-with-status :passed report))

(defmethod failed-tests ((report report))
  (tests-with-status :failed report))

(defmethod skipped-tests ((report report))
  (tests-with-status :skipped report))

(defmethod timing ((report report) &optional test)
  (timing (test-result report (or test (about report)))))

(defun test (test-name &rest args &key (report 'plain) &allow-other-keys)
  (let ((test (find-test test-name)))
    (run test (apply #'make-instance report :test test args))
    (report report report)))

(defmethod run :around ((test test) (report report))
  (unless (test-result report test)
    (call-next-method)))

(defmethod run :before ((test test) (report report))
  (dolist (dep (dependencies test))
    (run dep report)))

(defmethod run ((test test) (report report))
  (let ((*report* report)
        (result (make-instance 'result :test test)))
    (setf (test-result report test) result)
    (unless (loop for dep in (dependencies test)
                  thereis (eql :failed (test-status report dep)))
      (funcall (test-body test))
      (dolist (child (if (serial test)
                         (children test)
                         (shuffle (children test))))
        (run child report)))))

(defmethod run :after ((test test) (report report))
  (report (test-result report test) report))

(defvar *level* 0)

(defclass plain (report)
  ())

(defmethod run :around (test (report report))
  (let ((*level* (1+ *level*)))
    (call-next-method)))

(defmethod run (test (report report))
  (handler-case (call-next-method)
    (error (err)
      (declare (ignore err))
      (setf (status (test-result report test)) :failed))))

(defmethod report ((result result) (report plain))
  (format T "~& ~6,3f ~a~v@{    ~} ~a~%"
          (duration result)
          *level*
          (case (status result) (:passed "o") (:failed "x") (:skipped "."))
          (present report :oneline)))

(defmethod report ((a plain) (report plain))
  (format T "~&~%~
             ;; Summary:~%~
             Passed:  ~4d~%~
             Failed:  ~4d~%~
             Skipped: ~4d~%~
             ~%~
             ;; Failed tests:~%~
             ~{~a~%~}"
          (length (passed-tests report))
          (length (failed-tests report))
          (length (skipped-tests report))
          (loop for result in (results report)
                when (eql :failed (status result))
                collect (present result :extensive))))

(defclass interactive (report)
  ())

(defmethod run (test (report interactive))
  (restart-case
      (call-next-method)
    (retry ()
      :report (lambda (s) (format s "Retry running ~a." test))
      (run test report))
    (fail ()
      :report (lambda (s) (format s "Continue, failing ~a." test))
      (setf (status (test-result report test)) :failed))
    (pass ()
      :report (lambda (s) (format s "Continue, passing ~a." test))
      (setf (status (test-result report test)) :passed))
    (skip ()
      :report (lambda (s) (format s "Continue, skipping ~a." test))
      (setf (status (test-result report test)) :skipped))))

(defmethod report (result (report interactive)))

(defmethod report ((a interactive) (report interactive))
  (values (length (passed-tests report))
          (length (failed-tests report))
          (length (skipped-tests report))))

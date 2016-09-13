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
(defgeneric report (result report))

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
        collect (about result)))

(defmethod passed-tests ((report report))
  (tests-with-status :passed report))

(defmethod failed-tests ((report report))
  (tests-with-status :failed report))

(defmethod skipped-tests ((report report))
  (tests-with-status :skipped report))

(defmethod timing ((report report) &optional test)
  (timing (test-result report (or test (about report)))))

(defun test (test-name &rest args &key (report 'plain) &allow-other-keys)
  (let* ((test (find-test test-name))
         (*report* (apply #'make-instance report :test test args)))
    (run *report* test)
    (report *report* *report*)))

(defmethod run :around ((report report) (test test))
  (unless (test-result report test)
    (call-next-method)))

(defmethod run :before ((report report) (test test))
  (dolist (dep (dependencies test))
    (run report dep)))

(defmethod run ((report report) (test test))
  (flet ((maybe-shuffle (list)
           (if (serial test) list (shuffle list))))
    (let ((result (make-instance 'result :test test)))
      (setf (test-result report test) result)
      (cond ((loop for dep in (dependencies test)
                   thereis (failed-p (test-result report dep)))
             (setf (status result) :skipped))
            (T
             (dolist (test (maybe-shuffle (tests test)))
               (let ((result (run result test)))
                 (when (and (typep result 'result) (failed-p result))
                   (setf (status result) :failed))))
             (dolist (child (maybe-shuffle (children test)))
               (run report child))
             (unless (failed-p result)
               (setf (status result) :passed))))
      result)))

(defmethod run ((report report) (func function))
  (let ((result (make-instance 'result :test func)))
    (setf (test-result report func) result)
    (setf (status result)
          (if (run func result)
              :passed
              :failed))
    result))

(defmethod run :after ((report report) (test test))
  (report (test-result report test) report))

(defvar *level* 0)

(defclass plain (report)
  ())

(defmethod run ((report plain) test)
  (let ((*level* (1+ *level*)))
    (handler-case (call-next-method)
      (error (err)
        (declare (ignore err))
        (let ((result (test-result report test)))
          (setf (status result) :failed)
          result)))))

(defmethod report ((result result) (report plain))
  (format T "~& ~:[      ~;~:*~6,3f~] ~a~v@{    ~} ~a~%"
          (duration result)
          (case (status result)
            (:passed  #+asdf-unicode "✔" #-asdf-unicode "o")
            (:failed  #+asdf-unicode "✘" #-asdf-unicode "x")
            (:skipped #+asdf-unicode "ー" #-asdf-unicode "-")
            (T        #+asdf-unicode "？" #-asdf-unicode "?"))
          *level*
          (present result :oneline))
  (force-output))

(defmethod report ((a plain) (report plain))
  (format T "~&~%~
             ;; Summary:~%~
             Passed:  ~4d~%~
             Failed:  ~4d~%~
             Skipped: ~4d~%~
             ~@[~%~
             ;; Failed tests:~%~
             ~{~a~%~}~]"
          (length (passed-tests report))
          (length (failed-tests report))
          (length (skipped-tests report))
          (loop for result in (results report)
                when (failed-p result)
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
      (setf (status (test-result report test)) :skipped)))
  (test-result report test))

(defmethod report (result (report interactive)))

(defmethod report ((a interactive) (report interactive))
  (values (length (passed-tests report))
          (length (failed-tests report))
          (length (skipped-tests report))))

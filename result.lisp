#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

(defgeneric run (report test-ish))
(defgeneric present (report type))

(defclass result ()
  ((about :initarg :test :accessor about)
   (status :initarg :status :accessor status)
   (duration :initarg :duration :accessor duration)
   (description :initarg :description :accessor description))
  (:default-initargs
   :test (error "TEST required.")
   :status :running
   :duration NIL
   :description NIL))

(defmethod run ((result result) (func function))
  (let ((start (get-internal-real-time)))
    (unwind-protect
         (funcall func)
      (setf (duration result)
            (/ (- (get-internal-real-time) start)
               internal-time-units-per-second)))))

(defmethod present ((result result) (type (eql :oneline)))
  (typecase (about result)
    (test (name (about result)))
    (T (princ-to-string (about result)))))

(defmethod present ((result result) (type (eql :extensive)))
  (with-output-to-string (out)
    (format out "Running ~a ~(~a~)~@[ in ~,3fs~]."
            (present result :oneline)
            (status result)
            (duration result))
    (when (eql :failed (status result))
      (if (description result)
          (format out "~&The following was recorded as the failure reason:~%~%~a"
                  (description result))
          (format out "~&No additional information about the failure was given.")))))

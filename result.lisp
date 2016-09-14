#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

(defclass test-result ()
  ((expression :initarg :expression :accessor expression)
   (status :initarg :status :accessor status)
   (duration :initarg :duration :accessor duration)
   (description :initarg :description :accessor description)))

(defmethod passed-p ((result test-result))
  (eql :passed (status result)))

(defmethod failed-p ((result test-result))
  (eql :failed (status result)))

(defmethod skipped-p ((result test-result))
  (eql :skipped (status result)))

(defmethod run ((result test-result) (func function))
  (let ((start (get-internal-real-time)))
    (unwind-protect
         (funcall func)
      (setf (duration result)
            (/ (- (get-internal-real-time) start)
               internal-time-units-per-second))))
  result)

(defmethod present-object ((result test-result) (type (eql :oneline)) out)
  (typecase (expression result)
    (test (write-string (name (expression result)) out))
    (T (write (expression result) :stream out :case :downcase :escape NIL :readably NIL))))

(defmethod present-object ((result test-result) (type (eql :extensive)) out)
  (format out "Running ~a ~(~a~)~@[ in ~,3fs~]."
          (present result :oneline)
          (status result)
          (duration result))
  (when (eql :failed (status result))
    (when (description result)
      (format out "~&~a"
              (description result)))))

(defclass comparison-result (test-result)
  ((result :initarg :result :accessor resulr)
   (expected :initarg :expected :accessor expected)
   (comparison :initarg :comparison :accessor comparison)))

(defmethod run ((result comparison-result) thing)
  (setf (result result) (call-next-method)))

(defmethod run :after ((result comparison-result) thing)
  (setf (status result) (if (ignore-errors
                             (funcall (comparison result) (result result) (expected result)))
                            :passed :failed)))

(defmethod present-object :after ((result comparison-result) (type (eql :extensive)) out)
  (fresh-line out)
  (format out "The expression evaluated to~%  ~a
               with the expected result being~% ~a
               under the ~a comparator."
          (result result)
          (expected result)
          (comparison result)))

(defclass subtest-result (test-result)
  ((children :initarg :children :accessor children)))

(defmethod present-object :after ((result subtest-result) (type (eql :oneline)) out)
  (write (length (children result)) :stream out))

(defmethod present-object :after ((result subtest-result) (type (eql :extensive)) out)
  (let ((failed (loop for c in (children result) collect (failed-p c))))
    (format out "~&The result has ~d children, of which ~d failed~:[:~;.~]"
            (length (children result)) (length failed) (= 0 (length failed)))
    (dolist (child failed)
      (terpri out)
      (present-object result type out))))

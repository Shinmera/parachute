#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:org.shirakumo.parachute.5am
  (:nicknames #:5am #:parachute-5am)
  (:use #:cl)
  (:export
   #:*default-test-compilation-time*
   #:make-suite
   #:def-suite
   #:in-suite
   #:in-suite*
   #:test
   #:def-test
   #:get-test
   #:rem-test
   ;; fixtures
   #:def-fixture
   #:with-fixture
   #:get-fixture
   #:rem-fixture
   ;; running checks
   #:is
   #:is-true
   #:is-false
   #:signals
   #:finishes
   #:skip
   #:pass
   #:fail
   #:*test-dribble*
   #:for-all
   #:gen-integer
   #:gen-float
   #:gen-character
   #:gen-string
   #:gen-list
   #:gen-tree
   #:gen-buffer
   #:gen-one-element
   ;; running tests
   #:run
   #:explain
   #:explain!
   #:run!
   #:!
   #:!!
   #:!!!
   #:*debug-on-error*
   #:*debug-on-failure*
   #:*verbose-failures*
   #:*run-test-when-defined*))
(in-package #:org.shirakumo.parachute.5am)

(pushnew :5am *features*)

;; Just for compat.
(defvar *test-dribble* T)
(defvar *debug-on-error* NIL)
(defvar *debug-on-failure* NIL)
(defvar *verbose-failures* NIL)
(defvar *run-test-when-defined* NIL)
(defvar *suite* NIL)
(defvar *!* NIL)
(defvar *!!* NIL)
(defvar *!!!* NIL)
(defvar *home* (find-package '#:org.shirakumo.parachute.5am))
(defvar *num-trials* 100)
(defvar *default-test-compilation-time* :run-time)

(defun enlist (a &rest els)
  (if (listp a) a (list* a els)))

;; Tests
(defun make-suite (name &key description in)
  (make-instance 'parachute:test
                 :name name
                 :home *home*
                 :parent in
                 :description description))

(defmacro def-suite (name &key description in)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (parachute:define-test ,name
       :home *home*
       :description ,description
       :parent ,in)))

(defmacro in-suite (suite-name)
  ;; The eval-when is a departure from 5AM, but we have to do this as the
  ;; parent needs to be evaluated at compile-time in the test definition.
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (parachute:find-test ',suite-name *home*)
       (cerror "Create a new suite named ~a"
               "Unknown suite ~a." ',suite-name)
       (parachute:define-test ,suite-name))
     (setf *suite* ',suite-name)
     ',suite-name))

(defun get-test (key &optional default)
  (or (parachute:find-test key *home*)
      default))

(defun rem-test (key)
  (parachute:remove-test key *home*))

(defun test-names ()
  (mapcar #'parachute:name (parachute:package-tests *home*)))

(defmacro test (name &body body)
  (destructuring-bind (name &key depends-on (suite *suite*) fixture (compile-at *default-test-compilation-time*) profile) (enlist name)
    `(def-test ,name (:depends-on ,depends-on
                      :suite ,suite
                      :fixture ,fixture
                      :compile-at ,compile-at
                      :profile ,profile)
       ,@body)))

(defun convert-depends-on (depends-on)
  (destructuring-bind (logop &rest deps) depends-on
    (list* (ecase logop (and :and) (or :or) (not :not))
           (loop for dep in deps
                 collect (if (listp dep)
                             (convert-depends-on dep)
                             dep)))))

(defmacro def-test (name (&key depends-on (suite *suite*) fixture (compile-at *default-test-compilation-time*) profile) &body body)
  (declare (ignore profile))
  (let ((body (ecase compile-at
                (:definition-time body)
                (:run-time `((funcall (compile NIL '(lambda () ,@body))))))))
    `(progn
       (parachute:define-test ,name
         :home *home*
         :parent ,suite
         :depends-on ,(when depends-on (convert-depends-on depends-on))
         :description ,(form-fiddle:lambda-docstring `(lambda () ,@body))
         ,(if fixture
              (destructuring-bind (name &rest args) (enlist fixture)
                `(with-fixture ,name ,args ,@body))
              `(progn ,@body)))
       (when *run-test-when-defined*
         (run! ',name))
       ',name)))

;; Fixtures
(defvar *fixtures* (make-hash-table :test 'eql))

(defun get-fixture (key &optional default)
  (gethash key *fixtures* default))

(defun (setf get-fixture) (value key)
  (setf (gethash key *fixtures*) value))

(defmacro def-fixture (name args &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get-fixture ',name) '(,args ,@body))
     ',name))

(defmacro with-fixture (name args &body body)
  (or (get-fixture name) (error "Unknown fixture ~s." name))
  `(macrolet ((&body () '(progn ,@body)))
     (funcall (lambda ,@(get-fixture name)) ,@args)))

;; Checks
(defmacro is (test &rest reason-args)
  `(parachute:true ,test ,@reason-args))

(defmacro is-true (condition &rest reason)
  `(parachute:true ,condition ,@reason))

(defmacro is-false (condition &rest reason)
  `(parachute:false ,condition ,@reason))

(defmacro signals (condition-spec &body body)
  `(block NIL
     (parachute:fail (progn ,@body) ,condition-spec)))

(defmacro finishes (&body body)
  `(parachute:finish (progn ,@body)))

(defmacro skip (&rest reason)
  `(parachute:eval-in-context
    parachute:*context*
    (make-instance 'parachute:result
                   :status :skipped
                   :description (format NIL ,@reason))))

(defmacro pass (&rest reason)
  `(eval-in-context
    *context*
    (make-instance 'parachute:result
                   :status :passed
                   :description (format NIL ,@reason))))

(defmacro fail (&rest reason)
  `(parachute:eval-in-context
    parachute:*context*
    (make-instance 'parachute:result
                   :status :failed
                   :description (format NIL ,@reason))))

;; Generation
(defclass generation-result (parachute:parent-result)
  ((generators :initarg :generators :accessor generators)
   (guard :initarg :guard :accessor guard)
   (body :initarg :body :accessor body)))

(defmethod parachute:eval-in-context (context (result generation-result))
  (loop with failure = NIL
        for vars = (mapcar #'funcall (generators result))
        repeat *num-trials*
        do (setf (fill-pointer (parachute:children result)) 0)
           ;; in order to avoid processing "uninteresting" results,
           ;; we simply catch everything and rebind the context and
           ;; rerun the last trial with the proper context. This may
           ;; be problematic in the case of side-effects, but I can't
           ;; think of anything better right now.
           (let ((parachute:*context* result))
             (handler-case (apply (body result) vars)
               (error (err) (setf failure err))))
        until (or failure
                  (loop for child across (parachute:children result)
                        thereis (eql :failed (parachute:status child))))
        while (apply (guard result) vars)
        finally (apply (body result) vars)))

(defmacro for-all (bindings &body body)
  (let ((vars (mapcar #'first bindings)))
    `(parachute:eval-in-context
      parachute:*context*
      (make-instance 'generation-result
                     :expression '(for-all ,bindings ,@body)
                     :generators (list ,@(mapcar #'second bindings))
                     :guard (lambda ,vars
                              (declare (ignorable ,@vars))
                              (and ,@(remove NIL (mapcar #'third bindings))))
                     :body (lambda ,vars
                             ,@body)))))

(defun gen-integer (&key (max (1+ most-positive-fixnum))
                         (min (1- most-negative-fixnum)))
  (lambda () (+ min (random (1+ (- max min))))))

(defun gen-float (&key bound (type 'short-float))
  (let* ((most-negative (ecase type
                          (short-float most-negative-short-float)
                          (single-float most-negative-single-float)
                          (double-float most-negative-double-float)
                          (long-float most-negative-long-float)))
         (most-positive (ecase type
                          (short-float most-positive-short-float)
                          (single-float most-positive-single-float)
                          (double-float most-positive-double-float)
                          (long-float most-positive-long-float)))
         (bound (or (coerce bound type) (max most-positive (- most-negative)))))
    (lambda ()
      (coerce (ecase (random 2)
                (0 (random (min most-positive bound)))
                (1 (- (random (min (- most-negative) bound)))))
              type))))

(defun gen-character (&key (code-limit char-code-limit)
                           (code (gen-integer :min 0 :max (1- code-limit)))
                           (alphanumericp nil))
  (lambda ()
    (or (loop for count from 0 to 1000
              for char = (code-char (funcall code))
              when (and char (or (not alphanumericp) (alphanumericp char)))
              do (return char))
        (error "After 1000 iterations ~S has still not generated ~:[a valid~;an alphanumeric~] character :(."
               code alphanumericp))))

(defun gen-string (&key (length (gen-integer :min 0 :max 80))
                        (elements (gen-character))
                        (element-type 'character))
  (gen-buffer :length length :elements elements :element-type element-type))

(defun gen-list (&key (length (gen-integer :min 0 :max 10))
                      (elements (gen-integer :min -10 :max 10)))
  (lambda ()
    (loop repeat (funcall length)
          collect (funcall elements))))

(defun gen-tree (&key (size 20)
                      (elements (gen-integer :min -10 :max 10)))
  (labels ((build (&optional (current-depth 0))
             (let ((leaf (< (random (+ 3 (- size current-depth))) 2)))
               (if leaf
                   (funcall elements)
                   (list (build (+ current-depth 1))
                         (build (+ current-depth 1)))))))
    (lambda () (build))))

(defun gen-buffer (&key (length (gen-integer :min 0 :max 50))
                        (element-type '(unsigned-byte 8))
                        (elements (gen-integer :min 0 :max (1- (expt 2 8)))))
  (lambda ()
    (let ((buffer (make-array (funcall length) :element-type element-type)))
      (map-into buffer elements))))

(defun gen-one-element (&rest elements)
  (let ((len (length elements)))
    (lambda () (nth (random len) elements))))

;; Running
(defun test-results (report)
  (remove-if-not (lambda (a) (typep a 'parachute:test-result))
                 (parachute:children report)))

(defun run (test)
  (let ((test (or (parachute:find-test test *home*)
                  (error "No such test ~s." test))))
    (shiftf *!!!* *!!* *!* (lambda ()
                             (test-results (parachute:test test :report 'parachute:quiet))))
    (funcall *!*)))

(defun run! (test)
  (let ((test (or (parachute:find-test test *home*)
                  (error "No such test ~s." test))))
    (shiftf *!!!* *!!* *!* (lambda ()
                             (test-results (parachute:test test :report 'parachute:plain))))
    (funcall *!*)))

(defun explain! (result-list)
  (explain 'parachute:plain result-list))

(defun explain (report-type results &optional stream recursive-depth)
  (declare (ignore recursive-depth))
  (let ((report (make-instance report-type :expression '5am :stream stream)))
    (setf (parachute:children report) results)
    (parachute:summarize report)))

(defun ! () (funcall *!*))
(defun !! () (funcall *!!*))
(defun !!! () (funcall *!!!*))

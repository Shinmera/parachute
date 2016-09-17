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
   #:gen-string
   #:gen-character
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
  (destructuring-bind (name &key depends-on (suite *suite*)) (enlist name)
    `(def-test ,name (:depends-on ,depends-on :suite ,suite)
       ,@body)))

(defun convert-depends-on (depends-on)
  (destructuring-bind (logop &rest deps) depends-on
    (list* (ecase logop (and :and) (or :or) (not :not))
           (loop for dep in deps
                 collect (if (listp dep)
                             (convert-depends-on dep)
                             dep)))))

(defmacro def-test (name (&key depends-on (suite *suite*) fixture (compile-at :run-time) profile) &body body)
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

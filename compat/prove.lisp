#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:org.shirakumo.parachute.prove
  (:nicknames #:prove #:parachute-prove)
  (:use #:cl)
  (:export
   #:*debug-on-error*
   #:*test-result-output*
   #:*default-test-function*
   #:*default-reporter*
   #:*gensym-prefix*
   #:*default-slow-threshold*
   #:*suite*
   #:*enable-colors*
   #:test-file
   #:run-test-system
   #:run
   #:ok
   #:is
   #:isnt
   #:is-values
   #:is-print
   #:is-condition
   #:is-error
   #:is-type
   #:like
   #:is-expand
   #:diag
   #:skip
   #:pass
   #:fail
   #:subtest
   #:deftest
   #:run-test
   #:run-test-package
   #:run-test-all
   #:remove-test
   #:remove-test-all
   #:plan
   #:finalize
   #:slow-threshold
   #:current-suite
   #:reset-suite
   #:suite
   #:package-suite))
(in-package #:org.shirakumo.parachute.prove)

(defvar *test-result-output* T)
(defvar *suite* NIL)
;; These don't actually do anything
(defvar *debug-on-error* NIL)
(defvar *default-test-function* #'equal)
(defvar *default-reporter* 'parachute:plain)
(defvar *gensym-prefix* "$")
(defvar *default-slow-threshold* 75)
(defvar *enable-colors* NIL)
;; Internal
(defvar *system-test-files* (make-hash-table :test 'eq))
(defvar *defined-suites* (make-hash-table :test 'eq))

(defclass test-file (asdf:cl-source-file) ())
(import 'test-file :asdf)

(defmethod asdf:perform ((op asdf:compile-op) (c test-file)))

(defmethod asdf:perform ((op asdf:load-op) (c test-file))
  (pushnew (asdf:component-pathname c)
           (gethash (asdf:component-system c) *system-test-files*)))

(defun run-test-system (system-designator)
  #+quicklisp (ql:quickload (if (typep system-designator 'asdf:system)
                                (asdf:component-name system-designator)
                                system-designator))
  #-quicklisp (asdf:load-system system-designator)
  (let ((system (asdf:find-system system-designator)))
    (dolist (file (gethash system *system-test-files*))
      (load file :verbose T))))

(defun run (object &key reporter)
  (declare (ignore reporter))
  (etypecase object
    (string
     (if (asdf:find-system object NIL)
         (run-test-system object)
         (run (pathname object))))
    (pathname
     (cond ((uiop:directory-pathname-p object)
            (dolist (file (uiop:directory-files object (make-pathname :type "lisp" :defaults uiop:*wild-file*)))
              (run file)))
           (T
            (load object))))
    (T
     (run-test-system object))))

(defmacro capture-stdout (&body body)
  (let ((out (gensym "OUT")))
    `(with-output-to-string (,out)
       (let ((*standard-output* (make-broadcast-stream *standard-output* ,out)))
         ,@body))))

(defun parse-description-and-test (args)
  (if (consp args)
      (case (length args)
        (1 (car args))
        (2 (if (eq :test (car args))
               (values nil (cadr args))
               (car args)))
        (t (let ((k (member :test args)))
             (case (length k)
               ((0 1) (car args))
               (2 (values (car args) (cadr k)))
               (t (values (nth 2 k) (cadr k)))))))
      args))

(defmacro ok (test &optional description)
  `(parachute:true ,test "~a" ,description))

(defmacro is (got expected &rest args)
  (multiple-value-bind (desc test) (parse-description-and-test args)
    `(parachute:true (funcall ,test ,got ,expected) "~a" ,desc)))

(defmacro isnt (got expected &rest args)
  (multiple-value-bind (desc test) (parse-description-and-test args)
    `(parachute:false (funcall ,test ,got ,expected) "~a" ,desc)))

(defmacro is-values (got expected &rest args)
  `(is (multiple-value-list ,got) ,expected ,@args))

(defmacro is-print (got expected &optional desc)
  `(parachute:is equal ,expected (capture-stdout ,got) "~a" ,desc))

(defmacro is-condition (got condition &optional desc)
  `(parachute:fail ,got ,condition "~a" ,desc))

(defmacro is-error (got condition &optional desc)
  `(is-condition ,got ,condition ,desc))

(defmacro is-type (got expected &optional desc)
  `(parachute:is typep ,expected ,got "~a" ,desc))

(defmacro like (got regex &optional desc)
  `(parachute:is cl-ppcre:scan ,regex ,got "~a" ,desc))

(defmacro is-expand (got expected &optional desc)
  `(parachute:is equal ',expected (macroexpand-1 ',got) "~a" ,desc))

(defun diag (desc)
  (format (parachute:output parachute:*context*) "~& ==> ~a~%" desc))

(defun skip (how-many why &rest format-args)
  (diag (format NIL "Skipping the next ~a tests: ~?" how-many why format-args))
  (setf (to-skip parachute:*context*) how-many))

(defun pass (desc)
  `(parachute:eval-in-context
    parachute:*context*
    (make-instance 'parachute:result
                   :status :passed
                   :description ,desc)))

(defun fail (desc)
  `(parachute:eval-in-context
    parachute:*context*
    (make-instance 'parachute:result
                   :status :failed
                   :description ,desc)))

(defclass subtest (parachute:parent-result)
  ((body :initarg :body :accessor body)))

(defmethod parachute:eval-in-context (context (result subtest))
  (funcall (body result)))

(defmacro subtest (desc &body body)
  `(parachute:eval-in-context
    parachute:*context*
    (make-instance 'subtest
                   :expression ,desc
                   :body (lambda () ,@body))))

(defmacro deftest (name &body test-forms)
  `(parachute:define-test ,name
     ,@test-forms))

(defun run-test (name)
  (parachute:test name :output *test-result-output*))

(defun run-test-package (package)
  (parachute:test package :output *test-result-output*))

(defun run-test-all ()
  (loop for package being the hash-keys of parachute::*test-indexes*
        do (run-test-package package)))

(defun remove-test (name)
  (parachute:remove-test name *package*))

(defun remove-test-all ()
  (dolist (test (parachute:package-tests *package*))
    (parachute:remove-test test)))

;; That'll do, pig.
(defclass suite (parachute:plain)
  ((to-skip :initarg :to-skip :initform 0 :accessor to-skip)))

(defmethod parachute:eval-in-context ((report suite) (result parachute:result))
  (cond ((<= (to-skip report) 0)
         (call-next-method))
        (T
         (setf (parachute:status result) :skipped)
         (decf (to-skip report)))))

;; That'll do.
(defclass package-suite (suite)
  ())

(defun slow-threshold (&optional new-threshold)
  (declare (ignore new-threshold))
  ;; We don't really implement this.
  *default-slow-threshold*)

(defun find-package-suite (package)
  (let ((package (find-package package)))
    (or (gethash package *defined-suites*)
        (setf (gethash package *defined-suites*)
              (make-instance 'package-suite
                             :output *test-result-output*
                             :expression package)))))

(defun current-suite ()
  (or *suite* (find-package-suite *package*)))

(defun reset-suite (suite)
  (setf (parachute:duration suite) NIL)
  (setf (parachute:status suite) :unknown)
  (setf (fill-pointer (parachute:children suite)) 0))

(defun plan (n)
  ;; Still not sure what the number is even useful for.
  (declare (ignore n))
  (setf parachute:*context* (current-suite))
  (reset-suite parachute:*context*))

(defun finalize ()
  (parachute:summarize (current-suite))
  (setf parachute:*context* NIL))

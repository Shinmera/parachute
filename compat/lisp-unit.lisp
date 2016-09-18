#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:org.shirakumo.parachute.lisp-unit
  (:nicknames #:lisp-unit #:parachute-lisp-unit)
  (:use #:cl)
  (:export
   #:*print-summary*
   #:*print-failures*
   #:*print-errors*
   #:*summarize-results*)
  (:export
   #:assert-eq
   #:assert-eql
   #:assert-equal
   #:assert-equalp
   #:assert-equality
   #:assert-prints
   #:assert-expands
   #:assert-true
   #:assert-test
   #:assert-false
   #:assert-nil
   #:assert-error)
  (:export
   #:define-test
   #:list-tests
   #:test-code
   #:test-documentation
   #:remove-tests
   #:run-tests
   #:use-debugger)
  (:export
   #:list-tags
   #:tagged-tests
   #:remove-tags
   #:run-tags)
  (:export
   #:test-names
   #:failed-tests
   #:error-tests
   #:missing-tests
   #:print-failures
   #:print-errors
   #:summarize-results)
  (:export
   #:reduce-test-results-dbs)
  (:export
   #:signal-results
   #:test-run-complete
   #:results)
  (:export
   #:logically-equal
   #:set-equal))
(in-package #:org.shirakumo.parachute.lisp-unit)

;; These don't actually do anything.
(defvar *print-summary* T)
(defvar *print-failures* T)
(defvar *print-errors* T)
(defvar *summarize-results* T)
;; Internal management.
(defvar *report* 'plain)
(defvar *tags* (make-hash-table :test 'eq))

(defmacro capture-stdout (&body body)
  (let ((out (gensym "OUT")))
    `(with-output-to-string (,out)
       (let ((*standard-output* (make-broadcast-stream *standard-output* ,out)))
         ,@body))))

(defun expansion (form)
  (let ((*gensym-counter* 1))
    (macroexpand-1 form)))

(defmacro assert-eq (expected form)
  `(parachute:is eq ,expected ,form))

(defmacro assert-eql (expected form)
  `(parachute:is eql ,expected ,form))

(defmacro assert-equal (expected form)
  `(parachute:is equal ,expected ,form))

(defmacro assert-equalp (expected form)
  `(parachute:is equalp ,expected ,form))

(defmacro assert-error (condition form)
  `(parachute:fail ,form ,condition))

;; To be honest I don't know how this comparison could ever work
;; if it's by EQL, but that's what the source appears to do...
(defmacro assert-expands (expansion form)
  `(parachute:is eql ',expansion (expansion ',form)))

(defmacro assert-equality (test expected form)
  `(parachute:true (funcall ,test ,expected ,form)))

(defmacro assert-prints (output form)
  `(parachute:is equal ,output (capture-stdout ,form)))

(defmacro assert-nil (form)
  `(parachute:false ,form))

(defmacro assert-false (form)
  `(parachute:false ,form))

(defmacro assert-true (form)
  `(parachute:true ,form))

(defun parse-body (body)
  (let ((tags ())
        (doc NIL)
        (forms body))
    (loop for expr in body
          do (cond ((and (listp expr) (eql :tag (first expr)))
                    (pop forms)
                    (setf tags (append tags (rest expr))))
                   ((and (not doc) (rest forms) (stringp expr))
                    (pop forms)
                    (if tags
                        (return (values expr tags forms))
                        (setf doc expr)))
                   (T
                    (return (values doc tags forms)))))))

(defmacro define-test (name &body body)
  (let ((tag (gensym "TAG")))
    (multiple-value-bind (doc tags body) (parse-body body)
      `(progn (parachute:define-test ,name
                :description ,doc
                ,@body)
              (dolist (,tag ',tags)
                (pushnew ',name (gethash ,tag (tag-index *package*))))
              ',name))))

(defun list-tests (&optional (package *package*))
  (parachute:package-tests package))

(Defun test-code (name &optional (package *package*))
  (declare (ignore name package))
  NIL)

(defun test-documentation (name &optional (package *package*))
  (parachute:description
   (parachute:find-test name package)))

(defun remove-tests (&optional (names :all) (package *package*))
  (if (eq :all names)
      (if package
          (clrhash (gethash package parachute::*test-indexes* (make-hash-table)))
          (clrhash parachute::*test-indexes*))
      (dolist (name names)
        (parachute:remove-test name package))))

(defun run-tests (&optional (test-names :all) (package *package*))
  (parachute:test (if (eql :all test-names) package test-names) :report *report*))

(defun use-debugger (&optional (flag T))
  (setf *report* (if flag 'interactive 'report)))

(defun tag-index (package)
  (or (gethash package *tags*)
      (setf (gethash package *tags*) (make-hash-table :test 'eql))))

(defun list-tags (&optional (package *package*))
  (loop for k being the hash-keys of (tag-index package)
        collect k))

(defun tagged-tests (&optional (tags :all) (package *package*))
  (if (eql tags :all)
      (tagged-tests (list-tags package) package)
      (loop for tag in tags
            append (gethash tag (tag-index package)))))

(defun remove-tags (&optional (tags :all) (package *package*))
  (if (eql tags :all)
      (clrhash (tag-index package))
      (loop for tag in tags
            do (remhash tag (tag-index package)))))

(defun run-tags (&optional (tags :all) (package *package*))
  (run-tests (tagged-tests tags package) package))

(defun test-names ()
  (loop for index being the hash-values of parachute::*test-indexes*
        append (loop for test being the hash-values of index
                     collect (parachute:name test))))

(defun failed-tests (report)
  (parachute:tests-with-status :failed report))

(defun error-tests (report)
  (failed-tests report))

(defun missing-tests (report)
  (declare (ignore tests))
  NIL)

(defgeneric print-failures (result &optional stream)
  (:method (result &optional stream)
    (declare (ignore result stream))))

(defgeneric print-errors (result &optional stream)
  (:method (result &optional stream)
    (declare (ignore result stream))))

(defun summarize-results (results &optional (stream *standard-output*))
  (let ((report (make-instance 'parachute:plain :expression 'lisp-unit :stream stream)))
    (setf (parachute:children report) results)
    (parachute:summarize report)))

(defun reduce-test-results-dbs (all-results &key merge)
  (error "Not implemented."))

(defun signal-results (&optional (flag T))
  flag)

(define-condition test-run-complete ()
  ((results :initarg :results :reader results)))

(defun logically-equal (a b)
  (eql (not a) (not b)))

(defun set-equal (a b &key key (test #'equal))
  (and (listp a) (listp b)
       (subsetp a b :key key :test test)
       (subsetp b a :key key :test test)))

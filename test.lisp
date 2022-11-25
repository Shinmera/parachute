#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

(defvar *test-indexes* (make-hash-table :test 'eq))

(defvar *silence-plain-compilation-errors-p* T)
(defvar *abort-on-timeout-p* NIL)

(defclass test ()
  ((name :initarg :name :reader name)
   (home :initarg :home :reader home)
   (description :initarg :description :accessor description)
   (parent :initarg :parent :accessor parent)
   (children :initform NIL :accessor children)
   (dependencies :initarg :depends-on :initarg :dependencies :accessor referenced-dependencies)
   (fixtures :initarg :fix :initarg :fixtures :accessor fixtures)
   (time-limit :initarg :time-limit :accessor time-limit)
   (skipped-children :initarg :skip :initarg :skipped-children :accessor referenced-skips)
   (tests :initarg :tests :accessor tests)
   (serial :initarg :serial :accessor serial))
  (:default-initargs
   :name (error "NAME required.")
   :home *package*
   :description NIL
   :parent NIL
   :dependencies NIL
   :fixtures NIL
   :time-limit NIL
   :skipped-children NIL
   :serial T
   :tests ()))

(defmethod shared-initialize :after ((test test) slots &key parent home name)
  (declare (ignore slots))
  (when parent
    (let* ((home (if (listp parent) (first parent) home))
           (parent (if (listp parent) (second parent) parent))
           (found (find-test parent home)))
      (unless found
        (error "Could not find a parent by the name of ~a within ~a's home ~a!"
               parent name home))
      (setf (parent test) found)))
  ;; We dereference the dependencies at a later point so just warn for now.
  (handler-bind ((error (lambda (err)
                          (warn (princ-to-string err))
                          (continue err))))
    (dependencies test)))

(defmethod print-object ((test test) stream)
  (print-unreadable-object (test stream :type T)
    (format stream "~a::~a" (package-name (home test)) (name test))))

(defmethod dependencies ((test test))
  (let ((deps (referenced-dependencies test)))
    (unless (find (car deps) '(:and :or :not))
      (push :and deps))
    (when (parent test)
      (setf deps (list :and deps (dependencies (find-test (parent test))))))
    (resolve-dependency-combination deps test)))

(defmethod skipped-children ((test test))
  (loop for dep in (referenced-skips test)
        for (home name) = (if (listp dep) dep (list (home test) dep))
        for dependant-test = (find-test name home)
        when dependant-test
        collect dependant-test
        else
        do (warn "The reference to the child ~a of ~a cannot be found within ~a."
                 name (name test) home)))

(defmethod children :around ((test test))
  (if (serial test)
      (call-next-method)
      (shuffle (call-next-method))))

(defmethod tests :around ((test test))
  (if (serial test)
      (call-next-method)
      (shuffle (call-next-method))))

(defun test-index (name package-ish)
  (let ((package
          (etypecase name
            (test (home name))
            (symbol (or (find-package package-ish) (symbol-package name)))
            (string (if package-ish
                        (or (find-package package-ish)
                            (error "No such package ~a!" package-ish))
                        *package*)))))
    (values (gethash package *test-indexes*) package)))

(defun find-test (name &optional package-ish)
  (if (typep name 'test)
      name
      (let ((index (test-index name package-ish)))
        (or (when index (gethash (string name) index))
            (when (not package-ish) (find-test name *package*))))))

(defun (setf find-test) (test-instance name &optional package-ish)
  (multiple-value-bind (index package) (test-index name package-ish)
    (unless index
      (setf index (setf (gethash package *test-indexes*) (make-hash-table :test 'equal))))
    ;; Make sure to properly deregister test before adding a potentially new one.
    ;; The reason for this is that we want to ensure that if options were removed
    ;; that they are properly erased from the system wholly.
    (when (find-test name package)
      (remove-test name package))
    ;; Add the test to the children list directly. We can't do that in the class'
    ;; init function as then the child would be removed again in the above call.
    (when (parent test-instance)
      (setf (children (parent test-instance))
            (list* test-instance (remove (name test-instance) (children (parent test-instance))
                                         :key #'name :test #'equal))))
    (setf (gethash (string name) index) test-instance)))

(defun remove-test (name &optional package-ish)
  (let* ((test (or (find-test name package-ish)
                   (error "No such test ~a." name)))
         (parent (parent test))
         (index (test-index name (home test))))
    (remhash (string name) index)
    (when parent
      (setf (children parent) (remove test (children parent))))
    name))

(defun remove-all-tests-in-package (&optional (package *package*))
  (mapcar (lambda (x) (remove-test (name x) package))
          (package-tests package)))

(defun ensure-test (class &rest initargs)
  (let ((existing (find-test (getf initargs :name) (getf initargs :home)))
        (class (etypecase class
                 (class class)
                 (symbol (find-class class)))))
    (cond (existing
           (unless (eq (class-of existing) class)
             (apply #'change-class existing class initargs))
           (apply #'reinitialize-instance existing initargs))
          (T
           (apply #'make-instance class initargs)))))

(defmacro define-test (name &body arguments-and-body)
  (destructuring-bind (nparent name) (if (listp name) name (list NIL name))
    (form-fiddle:with-body-options (body options parent home (test-class 'test) (compile-at :compile-time)) arguments-and-body
      (let ((body (remove 'define-test body :key (lambda (a) (when (listp a) (car a))) :test #'eql))
            (defs (remove 'define-test body :key (lambda (a) (when (listp a) (car a))) :test-not #'eql))
            (home `(find-package ,(package-name (or home *package*)))))
        (when (and parent nparent)
          (error "Cannot specify parent through name and through a keyword argument at the same time!"))
        `(let ((*package* (find-package ,(package-name *package*)))) ; Make sure package stays consistent throughout initialisation.
           (setf (find-test ',name ,home)
                 (ensure-test ',test-class
                              :name ',name
                              :home ,home
                              :tests (list ,@(loop for form in body
                                                   collect (ecase compile-at
                                                             (:compile-time `(lambda () ,form))
                                                             (:execute `(lambda () (call-compile ',form))))))
                              :parent ',(or parent nparent)
                              ,@(loop for option in options
                                      collect `',option)))
           ,@(loop for (def subname . body) in defs
                   collect `(,def (,name ,subname)
                              :home ,home
                              ,@body))
           ',name)))))

(defmacro define-test+run (name &body args-and-body)
  `(progn
     (define-test ,name ,@args-and-body)
     (eval-when (:execute)
       (let* ((*silence-plain-compilation-errors-p* nil) ; must be let*! the order matters!
              (report (test ',name :report 'plain)))
         (if (member (status report) '(:passed :skipped))
             report
             (values report
                     (loop for result across (results report)
                           when (and (not (typep result 'test-result))
                                     (eql :failed (status result)))
                           collect (expression result))))))))

(defmacro define-test+run-interactively (name &body args-and-body)
  `(progn (define-test ,name ,@args-and-body)
          (eval-when (:execute)
            (values ',name (test ',name :report 'interactive)))))

(defun test-packages ()
  (loop for k being the hash-keys of *test-indexes*
        collect k))

(defun package-tests (package)
  (let* ((package (or (find-package package)
                      (error "Couldn't find a package called ~s." package)))
         (index (gethash package *test-indexes*)))
    (when index
      (loop for v being the hash-values of index
            collect v))))

(defmethod check-evaluatable (context (test test)))

(defmethod eval-in-context :around (context (test test))
  (let ((*package* (home test)))
    (with-fixtures (fixtures test)
      (call-next-method))))

(defmethod eval-in-context (context (test test))
  (if (and *abort-on-timeout-p* (time-limit test))
      (with-timeout (time-limit test)
        (loop for test in (tests test)
              do (funcall test)))
      (loop for test in (tests test)
            do (funcall test))))

(defmethod eval-in-context :after (context (test test))
  (loop with skipped = (skipped-children test)
        for child in (children test)
        for subresult = (result-for-testable child context)
        do (when (find child skipped)
             (setf (status subresult) :skipped))
           (eval-in-context context subresult)))

(defun resolve-dependency-combination (combination test)
  (destructuring-bind (logop &rest combinations) combination
    (let ((parents (loop for parent = test then (parent parent)
                         while parent
                         collect parent)))
      (flet ((find-test (name home)
               (let ((dep (find-test name home)))
                 (cond ((null dep)
                        (cerror "Ignore the dependency."
                                "The reference to the dependency ~a of ~a cannot be found within ~a."
                                name (name test) home))
                       ((find dep parents)
                        (cerror "Ignore the dependency."
                                "Cannot depend on test ~a as it is an ancestor of ~a."
                                dep test))
                       (T
                        dep)))))
        (list* logop
               (loop for comb in combinations
                     for dep = (if (listp comb)
                                   (cond ((find (first comb) '(:and :or :not))
                                          (resolve-dependency-combination comb test))
                                         ((= 2 (length comb))
                                          (find-test (second comb) (first comb)))
                                         (T (cerror "Ignore" "Malformed dependency spec: ~s" comb)))
                                   (find-test comb (home test)))
                     when dep collect dep))))))

(defun eval-dependency-combination (context combination)
  (destructuring-bind (logop &rest combinations) combination
    (assert (find logop '(:and :or :not)))
    (dolist (comb combinations)
      (etypecase comb
        (list (eval-dependency-combination context comb))
        (test (eval-in-context context (result-for-testable comb context)))))))

(defun check-dependency-combination (status context combination)
  (flet ((check (comb)
           (etypecase comb
             (list (check-dependency-combination status context comb))
             (test (eql status (status (find-child-result comb context)))))))
    (destructuring-bind (logop &rest combinations) combination
      (ecase logop
        (:and (loop for comb in combinations always (check comb)))
        (:or  (loop for comb in combinations thereis (check comb)))
        (:not (loop for comb in combinations never (check comb)))))))

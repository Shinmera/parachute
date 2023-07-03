(in-package #:org.shirakumo.parachute)

(defvar *parent* NIL)
(defvar *context* NIL)

(defmethod eval-in-context :around (context thing)
  (let ((*context* context))
    (call-next-method)))

(defmethod result-for-testable ((test test) context)
  (make-instance 'test-result :expression test))

(defclass result ()
  ((expression :initarg :expression :accessor expression)
   (status :initarg :status :accessor status)
   (duration :initarg :duration :accessor duration)
   (description :initarg :description :accessor description))
  (:default-initargs
   :expression (error "EXPRESSION required.")
   :status :unknown
   :duration NIL
   :description NIL))

(defmethod initialize-instance :after ((result result) &key)
  (when *parent* (add-result result *parent*))
  (when *context* (add-result result *context*)))

(defmethod print-object ((result result) stream)
  (print-unreadable-object (result stream :type T)
    (format stream "~s ~a" (status result) (format-result result :oneline))))

(defmethod format-result ((result result) (type (eql :oneline)))
  (print-oneline (expression result) NIL))

(defmethod format-result ((result result) (type (eql :extensive)))
  (format NIL "Test for ~a ~(~a~).~@[~%~a~]"
          (expression result) (status result)
          (description result)))

(defmethod check-evaluatable (context (result result)))

(defmethod eval-in-context :around (context (result result))
  ;; Unless the status is unknown marked we should probably skip.
  (when (eql :unknown (status result))
    (setf (status result) :tentative)
    ;; Make sure we are evaluatable.
    (check-evaluatable context result)
    (multiple-value-prog1
        (let ((start (get-internal-real-time)))
          (unwind-protect
               (handler-case (call-next-method)
                 (timeout ()))
            (setf (duration result) (/ (- (get-internal-real-time) start)
                                       internal-time-units-per-second))))
      ;; Mark ourselves as passed if we didn't already set the status.    
      (unless (result-complete-p result)
        (setf (status result) :passed)))))

(defmethod result-complete-p ((result result))
  (let ((status (status result)))
    (not (or (eql :unknown status)
             (eql :tentative status)))))

(defclass value-result (result)
  ((value :initarg :value :accessor value)
   (body :initarg :body :accessor body))
  (:default-initargs
   :body (error "BODY required.")))

(defmethod eval-in-context (context (result value-result))
  (unless (slot-boundp result 'value)
    (let ((results (multiple-value-list
                    (typecase (body result)
                      (function (funcall (body result)))
                      (T (body result))))))
      (setf (value result) (first results))
      (values-list results))))

(defclass multiple-value-result (value-result)
  ((value :initarg :value :accessor value)
   (body :initarg :body :accessor body))
  (:default-initargs
   :body (error "BODY required.")))

(defmethod eval-in-context (context (result multiple-value-result))
  (unless (slot-boundp result 'value)
    (setf (value result) (multiple-value-list
                          (typecase (body result)
                            (function (funcall (body result)))
                            (T (body result)))))
    (values-list (value result))))

(defclass finishing-result (value-result)
  ())

(defmethod eval-in-context (context (result finishing-result))
  (unwind-protect
       (handler-bind ((condition (lambda (err)
                                   (setf (value result) err))))
         (multiple-value-prog1
             (call-next-method)
           (setf (value result) NIL)
           (unless (result-complete-p result)
             (setf (status result) :passed))))
    (unless (result-complete-p result)
      (setf (status result) :failed))))

(defmethod format-result ((result finishing-result) (type (eql :extensive)))
  (with-output-to-string (out)
    (let ((*print-right-margin* 600))
      (format out "The test form   ~a~%"
              (print-oneline (expression result) NIL))
      (cond ((not (slot-boundp result 'value))
             (format out "exited with an unknown cause."))
            ((not (value result))
             (format out "finished without issue."))
            (T
             (format out "exited by       ~a" (print-oneline (value result) NIL))))
      (format out "~@[~&~a~]" (description result)))))

(defclass comparison-result (value-result)
  ((value-form :initarg :value-form :accessor value-form)
   (expected :initarg :expected :accessor expected)
   (comparison :initarg :comparison :accessor comparison)
   (comparison-geq :initarg :comparison-geq :accessor comparison-geq))
  (:default-initargs
   :value-form :unknown
   :expected '(not null)
   :comparison 'typep
   :comparison-geq T))

(defmethod format-result ((result comparison-result) (type (eql :extensive)))
  (let ((*print-right-margin* 600))
    (format NIL "The test form ~16t~a~%~
                 evaluated to ~16t~a~%~
                 when ~16t~a~%~
                 was expected to be ~:[unequal~;equal~] under ~a.~
                 ~@[~%~a~]"
            (print-oneline (value-form result) NIL)
            (print-oneline (if (slot-boundp result 'value)
                               (value result)
                               (make-symbol "#<NO RESULT>")) NIL)
            (print-oneline (expected result) NIL)
            (comparison-geq result)
            (comparison result)
            (description result))))

(defgeneric value-expected-p (result value expected))

(defmethod value-expected-p ((result comparison-result) value expected)
  (ignore-errors (geq (funcall (comparison result) value expected)
                      (comparison-geq result))))

(defmethod check-evaluatable (context (result comparison-result))
  (unless (or (functionp (comparison result))
              (fboundp (comparison result)))
    (error "~s is not a function designator and cannot be used for comparison."
           (comparison result))))

(defmethod eval-in-context (context (result comparison-result))
  (multiple-value-prog1
      (call-next-method)
    (unless (result-complete-p result)
      (if (value-expected-p result (value result) (expected result))
          (setf (status result) :passed)
          (setf (status result) :failed)))))

(defclass multiple-value-comparison-result (comparison-result multiple-value-result)
  ()
  (:default-initargs
   :expected '((not null))
   :comparison '(typep)))

(defmethod format-result ((result multiple-value-comparison-result) (type (eql :extensive)))
  (let* ((*print-right-margin* 600)
         (values (value result))
         (expecteds (expected result))
         (comparisons (comparison result))
         (max (max (length values) (length expecteds))))
    (setf comparisons (replace (make-list max :initial-element '#:<NONE>) comparisons))
    (setf expecteds (replace (make-list max :initial-element '#:<NO-VALUE>) expecteds))
    (setf values (replace (make-list max :initial-element '#:<NO-VALUE>) values))
    (print (list values expecteds))
    (format NIL "The test form ~16t~a~{~%~{~
                 ~d~a value is ~16t~a~%~
                 when ~16t~a~%~
                 was expected to be ~:[unequal~;equal~] under ~a.~}~}~
                 ~@[~%~a~]"
            (print-oneline (value-form result) NIL)
            (loop for i from 1
                  for comparison in comparisons
                  for value in values
                  for expected in expecteds
                  unless (ignore-errors
                          (geq (funcall comparison value expected)
                               (comparison-geq result)))
                  collect (list
                           i (number-suffix i)
                           (print-oneline value NIL)
                           (print-oneline expected NIL)
                           (comparison-geq result)
                           comparison))
            (description result))))

(defmethod value-expected-p ((result multiple-value-comparison-result) value expected)
  (or (and (null value) (null expected))
      (and (not (null value)) (not (null expected))
           (loop for comparison in (comparison result)
                 for value on value
                 for expected on expected
                 always (and (ignore-errors
                              (geq (funcall comparison (car value) (car expected))
                                   (comparison-geq result)))
                             (if (null (cdr value))
                                 (null (cdr expected))
                                 (not (null (cdr expected)))))))))

(defmethod check-evaluatable (context (result multiple-value-comparison-result))
  (dolist (comparison (comparison result))
    (unless (or (functionp comparison)
                (fboundp comparison))
      (error "~s is not a function designator and cannot be used for comparison."
             comparison))))

(defclass parent-result (result)
  ((results :initform (make-array 0 :adjustable T :fill-pointer T) :accessor results)))

(defmethod result-for-testable ((test test) (result parent-result))
  (or (find-child-result test result)
      (call-next-method)))

(defmethod eval-in-context :around (context (result parent-result))
  (let ((*parent* result))
    (call-next-method)))

(defmethod eval-in-context :after (context (result parent-result))
  (when (loop for child across (results result)
              thereis (eql :failed (status child)))
    (setf (status result) :failed)))

(defmethod find-child-result (test (result parent-result))
  (find test (results result) :key #'expression :test #'eq))

(defmethod results-with-status (status (result parent-result))
  (loop for result across (results result)
        when (eql status (status result))
        collect result))

(defmethod add-result ((result result) (parent parent-result))
  (vector-push-extend result (results parent)))

(defclass group-result (parent-result)
  ((body :initarg :body :accessor body)))

(defmethod check-evaluatable (context (result group-result))
  (check-type (expression result) symbol))

(defmethod format-result ((result group-result) (type (eql :oneline)))
  (format NIL "~a::~a"
          (package-name (symbol-package (expression result)))
          (symbol-name (expression result))))

(defmethod format-result ((result group-result) (type (eql :extensive)))
  (format NIL "~4d/~4d tests failed in ~a~@[~%~a~]"
          (length (results-with-status :failed result)) (length (results result))
          (format-result result :oneline)
          (description result)))

(defmethod eval-in-context (context (result group-result))
  (funcall (body result)))

(defclass test-result (parent-result)
  ())

(defmethod format-result ((result test-result) (type (eql :oneline)))
  (let ((name (name (expression result))))
    (format NIL "~a::~a"
            (package-name (home (expression result)))
            (if (symbolp name) (symbol-name name) name))))

(defmethod format-result ((result test-result) (type (eql :extensive)))
  (format NIL "~4d/~4d tests failed in ~a~@[~%~a~]"
          (length (results-with-status :failed result)) (length (results result))
          (format-result result :oneline)
          (description result)))

(defmethod eval-in-context :around (context (result test-result))
  ;; We have to run the dependencies here as they need to run before
  ;; the timing grips in the AROUND method of the RESULT class for
  ;; EVAL-IN-CONTEXT, which would count them running in a BEFORE.
  (eval-dependency-combination context (dependencies (expression result)))
  (multiple-value-prog1
      (call-next-method)
    (let ((test (expression result)))
      (when (and (time-limit test)
                 (< (time-limit test)
                    (duration result)))
        (setf (description result)
              (format NIL "The limit of ~fs was exceeded as the test took ~fs to run."
                      (time-limit test) (duration result)))
        (setf (status result) :failed)))))

(defmethod eval-in-context (context (result test-result))
  (let* ((test (expression result))
         (result (result-for-testable test context)))
    (setf (description result) (description test))
    (cond ((check-dependency-combination :passed context (dependencies test))
           (eval-in-context context test))
          (T
           (setf (status result) :skipped)))))

;; This is a hack, oh boy.
(defvar *real-context* NIL)

(defclass controlling-result (parent-result)
  ((child-status :initarg :child-status :accessor child-status)
   (body :initarg :body :accessor body)))

(defmethod eval-in-context (context (result controlling-result))
  (let ((*real-context* context)
        (*context* result))
    (funcall (body result))))

(defmethod eval-in-context :after (context (result controlling-result))
  (setf (status result) :passed))

(defmethod eval-in-context :before ((context controlling-result) (result result))
  (add-result result *real-context*))

(defmethod eval-in-context ((context controlling-result) (result value-result))
  (setf (body result) (lambda () (setf (status result) (child-status context))))
  (eval-in-context *real-context* result)
  (slot-makunbound result 'value)
  NIL)

(defmethod format-result ((result controlling-result) (type (eql :oneline)))
  (format NIL "~a~@[: ~a~]"
          (case (child-status result) (:skipped :skip) (:failed :fail) (:passed :pass) (T (child-status result)))
          (description result)))

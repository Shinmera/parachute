(in-package #:org.shirakumo.parachute)

(defun featurep (expr)
  (etypecase expr
    (symbol (find expr *features* :test #'string=))
    (cons (ecase (first expr)
            (and (loop for subexpr in (rest expr)
                       always (featurep subexpr)))
            (or (loop for subexpr in (rest expr)
                      thereis (featurep subexpr)))
            (not (not (featurep (second expr))))))))

(defun number-suffix (n)
  (case n
    (1 "st")
    (2 "nd")
    (3 "rd")
    (t "th")))

(defun shuffle (seq)
  (let ((seq (copy-seq seq))
        (len (length seq)))
    (dotimes (i len seq)
      (let ((r (+ i (random (- len i)))))
        (rotatef (elt seq i) (elt seq r))))))

(defmacro with-shuffling (&body body)
  (let ((thunk (gensym "THUNK")))
    `(dolist (,thunk (shuffle
                      (list ,@(loop for form in body
                                    collect `(lambda () ,form)))))
       (funcall ,thunk))))

(defun read-symbol (string &optional (package *package*))
  (intern (ecase (readtable-case *readtable*)
            (:upcase (string-upcase string))
            (:downcase (string-downcase string))
            (:preserve string)
            (:invert (map 'string (lambda (c) (if (upper-case-p c) (char-downcase c) (char-upcase c))) string)))
          package))

#+sbcl
(deftype timeout () 'sb-ext:timeout)
#-sbcl
(define-condition timeout (error) ())

(defmacro with-timeout (timeout &body body)
  #+(and clisp mt)
  `(mt:with-timeout (,timeout (error 'timeout))
     ,@body)
  #+(or allegro cmucl)
  `(mp:with-timeout (,timeout (error 'timeout))
     ,@body)
  #+sbcl
  `(sb-ext:with-timeout ,timeout
     ,@body)
  #-(or allegro (and clisp mt) cmucl sbcl)
  `(progn
     ,@body))

(defun removef (place &rest indicators)
  (loop for (k v) on place by #'cddr
        for found = (find k indicators)
        unless found collect k
        unless found collect v))

(defun locked-package-p (package)
  #+sbcl (sb-ext:package-locked-p package)
  #-sbcl (eql (find-package :cl) package))

(defun print-oneline (thing &optional (output T))
  (let ((*print-case* :downcase))
    (typecase output
      ((eql T)   (print-oneline thing *standard-output*))
      ((eql NIL) (with-output-to-string (o)
                   (print-oneline thing o)))
      (stream
       (typecase thing
         ((or string character keyword pathname)
          (prin1 thing output))
         (null
          (format output "()"))
         (cons
          (cond ((eql 'quote (first thing))
                 (format output "'")
                 (print-oneline (second thing) output))
                ((eql 'function (first thing))
                 (format output "#'")
                 (print-oneline (second thing) output))
                (*print-circle*
                 ;; Anything is better than dying.  Anything.
                 (let ((*print-lines* 1))
                   (princ thing output)))
                (T
                 (format output "(")
                 (loop for (car . cdr) on thing
                       do (print-oneline car output)
                          (typecase cdr
                            (null)
                            (cons (format output " "))
                            (T (format output " . ")
                             (print-oneline cdr output))))
                 (format output ")"))))
         (vector
          (format output "#(")
          (loop for i from 0 below (length thing)
                do (print-oneline (aref thing i) output)
                   (when (< i (1- (length thing)))
                     (format output " ")))
          (format output ")"))
         (condition
          (format output "[~a] ~a" (type-of thing) thing))
         (T
          (princ thing output)))))))

(defun geq (value expected)
  (if expected
      value
      (not value)))

(declaim (notinline retain))
(defun retain (argument)
  (declare (ignore argument))
  NIL)

(defmacro capture-error (form &optional (condition 'error))
  (let ((err (gensym "ERR")))
    `(handler-case
         (retain ,form)
       (,condition (,err)
         ,err))))

(defun maybe-quote (expression)
  (if (or (listp expression) (constantp expression))
      expression
      `',expression))

;;; THIS IS A BAD IDEA AND I DON'T REMEMBER WHY I ADDED IT.
(defun maybe-unquote (expression)
  (typecase expression
    (cons
     (if (eql (first expression) 'quote)
         (second expression)
         expression))
    (T expression)))

(defun call-compile (form)
  (multiple-value-bind (func warn-p fail-p fail-error) (try-compile form)
    (declare (ignore warn-p))
    (if fail-p
        (error "The test form failed to compile~@[~%~a~]" fail-error)
        (funcall func))))

(defun try-compile (form &optional (intercept 'error))
  ;; This whole rigamarole is to intercept compilation errors.
  (restart-case
      (flet ((handle (error previous)
               (declare (ignore previous))
               #+sbcl (setf error (first (simple-condition-format-arguments error)))
               (invoke-restart 'bail NIL NIL T error)))
        (trivial-custom-debugger:with-debugger (#'handle)
          (let ((*break-on-signals* intercept)
                (*error-output* (make-broadcast-stream)))
            (handler-bind (((or warning #+sbcl sb-ext:compiler-note) #'muffle-warning))
              (compile NIL `(lambda () ,form))))))
    (bail (&rest args)
      (values-list args))))

(defvar *status-indicators*
  '(:passed    #+asdf-unicode "✔" #-asdf-unicode "o"
    :failed    #+asdf-unicode "✘" #-asdf-unicode "x"
    :skipped   #+asdf-unicode "ー" #-asdf-unicode "-"
    :tentative #+asdf-unicode "？" #-asdf-unicode "?"
    :unknown   #+asdf-unicode "？" #-asdf-unicode "?"))

(defun status-character (status)
  (getf *status-indicators* status "?"))

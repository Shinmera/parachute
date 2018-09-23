#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

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
         (T
          (princ thing output)))))))

(defun geq (value expected)
  (if expected
      value
      (not value)))

(defmacro capture-error (form &optional (condition 'error))
  (let ((err (gensym "ERR")))
    `(handler-case
         (and ,form NIL)
       (,condition (,err)
         ,err))))

(defun maybe-quote (expression)
  (typecase expression
    (list (case (first expression)
            ((quote lambda function #+sbcl sb-int:quasiquote #+ecl si:quasiquote)
             expression)
            (T `',expression)))
    (T (if (constantp expression)
           expression
           `',expression))))

;;; THIS IS A BAD IDEA AND I DON'T REMEMBER WHY I ADDED IT.
(defun maybe-unquote (expression)
  (typecase expression
    (cons
     (if (eql (first expression) 'quote)
         (second expression)
         expression))
    (T expression)))

(defun call-compile (form)
  (handler-bind (((or warning #+sbcl sb-ext:compiler-note) #'muffle-warning))
    (funcall (compile NIL `(lambda () ,form)))))

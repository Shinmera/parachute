#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

(defvar *fixture-captures* (make-hash-table :test 'eql))
(defvar *fixture-restores* (make-hash-table :test 'eql))

(defmacro define-fixture-capture (name args &body body)
  `(progn (setf (gethash ',name *fixture-captures*)
                (lambda ,args ,@body))
          ',name))

(defmacro define-fixture-restore (name args &body body)
  `(progn (setf (gethash ',name *fixture-restores*)
                (lambda ,args ,@body))
          ',name))

(define-fixture-capture variable (symbol)
  (when (and (boundp symbol)
             (not #+:lispworks (sys:symbol-constant-p symbol)
                  #-:lispworks (constantp symbol)))
    (values (symbol-value symbol) T)))

(define-fixture-restore variable (symbol value)
  (setf (symbol-value symbol) value))

(define-fixture-capture macro (symbol)
  (when (and (macro-function symbol)
             (not (locked-package-p (symbol-package symbol))))
    (values (macro-function symbol) T)))

(define-fixture-restore macro (symbol value)
  (setf (macro-function symbol) value))

(define-fixture-capture function (symbol)
  (when (and (fboundp symbol)
             (not (macro-function symbol))
             (not (locked-package-p (symbol-package symbol))))
    (values (fdefinition symbol) T)))

(define-fixture-restore function (symbol value)
  (setf (fdefinition symbol) value))

(defun package-fixtures (name)
  (let* ((package (or (find-package name)
                      (error "No package with the name ~s found." name)))
         (symbols (loop for s being the symbols of package
                        collect s)))
    (mapcan #'capture-fixtures symbols)))

(defun capture-fixtures (fixture)
  (etypecase fixture
    (symbol
     (if (and (not (keywordp fixture))
              (symbol-package fixture))
         (list (list*
                fixture
                (loop for type being the hash-keys of *fixture-captures*
                      for (value captured-p) = (multiple-value-list
                                                (funcall (gethash type *fixture-captures*) fixture))
                      when captured-p collect (cons type value))))
         (package-fixtures fixture)))
    ((or string package)
     (package-fixtures fixture))))

(defun restore-fixtures (fixtures)
  (loop for (fixture . value) in fixtures
        do (loop for (type . value) in value
                 do (funcall (gethash type *fixture-restores*) fixture value))))

(defun call-with-fixtures (function fixtures)
  (let ((values (mapcan #'capture-fixtures fixtures)))
    (unwind-protect
         (funcall function)
      (restore-fixtures values))))

(defmacro with-fixtures (fixtures &body body)
  `(call-with-fixtures (lambda () ,@body) ,fixtures))

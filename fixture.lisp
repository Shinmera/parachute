#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

(defvar *fixture-capture-hooks* (make-hash-table :test 'eql))
(defvar *fixture-restore-hooks* (make-hash-table :test 'eql))

(defun package-fixture (name)
  (let* ((package (or (find-package name)
                      (error "No package with the name ~s found." name)))
         (symbols (loop for s being the symbols of package
                        collect s)))
    (mapcan #'fixture-values symbols)))

(defmacro define-fixture-capture-hook (name args &body body)
  `(progn (setf (gethash ',name *fixture-capture-hooks*)
                (lambda ,args ,@body))
          ',name))

(defmacro define-fixture-restore-hook (name args &body body)
  `(progn (setf (gethash ',name *fixture-restore-hooks*)
                (lambda ,args ,@body))
          ',name))

(define-fixture-capture-hook variable (symbol)
  (when (and (boundp symbol)
             (not #+:lispworks (sys:symbol-constant-p symbol)
                  #-:lispworks (constantp symbol)))
    (values (symbol-value symbol) T)))

(define-fixture-restore-hook variable (symbol value)
  (setf (symbol-value symbol) value))

(define-fixture-capture-hook macro (symbol)
  (when (and (macro-function symbol)
             (not (locked-package-p (symbol-package symbol))))
    (values (macro-function symbol) T)))

(define-fixture-restore-hook macro (symbol value)
  (setf (macro-function symbol) value))

(define-fixture-capture-hook function (symbol)
  (when (and (fboundp symbol)
             (not (macro-function symbol))
             (not (locked-package-p (symbol-package symbol))))
    (values (fdefinition symbol) T)))

(define-fixture-restore-hook function (symbol value)
  (setf (fdefinition symbol) value))

(defun fixture-values (fixture)
  (etypecase fixture
    (symbol
     (if (and (not (keywordp fixture))
              (symbol-package fixture))
         (list (list*
                fixture
                (loop for type being the hash-keys of *fixture-capture-hooks*
                      for (value captured-p) = (funcall (gethash type *fixture-capture-hooks*) fixture)
                      when captured-p collect (cons type value))))
         (package-fixture fixture)))
    (string
     (package-fixture fixture))))

(defun restore-fixtures (fixtures)
  (loop for (fixture . value) in fixtures
        do (loop for (type value) in value
                 do (funcall (gethash type *fixture-restore-hooks*) fixture value))))

(defun call-with-fixtures (function fixtures)
  (let ((values (mapcan #'fixture-values fixtures)))
    (unwind-protect
         (funcall function)
      (restore-fixtures values))))

(defmacro with-fixture (fixtures &body body)
  `(call-with-fixtures (lambda () ,@body) ,fixtures))

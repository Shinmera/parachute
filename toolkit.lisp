#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

(defun shuffle (seq)
  (let ((seq (copy-seq seq))
        (len (length seq)))
    (dotimes (i len seq)
      (let ((r (+ i (random (- len i)))))
        (rotatef (elt seq i) (elt seq r))))))

(defun removef (place &rest indicators)
  (loop for (k v) on place by #'cddr
        for found = (find k indicators)
        unless found collect k
        unless found collect v))

(defun package-fixture (name)
  (let* ((package (or (find-package name)
                      (error "No package with the name ~s found." name)))
         (symbols (loop for s being the symbols of package
                        collect s)))
    (mapcan #'fixture-values symbols)))

(defun symbol-fixture (symbol)
  ;; Can't use constants.
  (nconc
   (list symbol)
   (when (and (boundp symbol)
              (not #+:lispworks (sys:symbol-constant-p symbol)
                   #-:lispworks (constantp symbol)))
     (list :variable (symbol-value symbol)))
   (when (fboundp symbol)
     (if (macro-function symbol)
         (list :macro (macro-function symbol))
         (list :function (fdefinition symbol))))))

(defun fixture-values (fixture)
  (etypecase fixture
    (symbol
     (if (and (not (keywordp fixture))
              (symbol-package fixture))
         ;; We want to avoid package lock clashes.
         (unless #+sbcl (sb-ext:package-locked-p (symbol-package fixture))
                 #-sbcl (eql (find-package :cl) (symbol-package fixture))
                 (list (symbol-fixture fixture)))
         (package-fixture fixture)))
    (string
     (package-fixture fixture))))

(defun restore-fixtures (fixtures)
  (loop for (fixture . value) in fixtures
        do (loop for (k v) on value by #'cddr
                 do (case k
                      (:variable (setf (symbol-value fixture) v))
                      (:macro (setf (macro-function fixture) v))
                      (:function (setf (fdefinition fixture) v))))))

(defun call-with-fixtures (function fixtures)
  (let ((values (mapcan #'fixture-values fixtures)))
    (unwind-protect
         (funcall function)
      (restore-fixtures values))))

(defmacro with-fixture (fixtures &body body)
  `(call-with-fixtures (lambda () ,@body) ,fixtures))

#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem parachute-lisp-unit
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Parachute's lisp-unit compatibility layer."
  :homepage "https://github.com/Shinmera/parachute"
  :serial T
  :components ((:file "lisp-unit"))
  :depends-on (:parachute))

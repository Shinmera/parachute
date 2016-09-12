#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem parachute
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An extensible and cross-compatible testing framework."
  :homepage "https://github.com/Shinmera/parachute"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "test")
               (:file "report")
               ;(:file "tester")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :form-fiddle))

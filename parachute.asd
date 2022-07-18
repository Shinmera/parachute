#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem parachute
  :version "1.5.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An extensible and cross-compatible testing framework."
  :homepage "https://Shinmera.github.io/parachute/"
  :bug-tracker "https://github.com/Shinmera/parachute/issues"
  :source-control (:git "https://github.com/Shinmera/parachute.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "fixture")
               (:file "test")
               (:file "result")
               (:file "tester")
               (:file "report")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :trivial-custom-debugger
               :form-fiddle))

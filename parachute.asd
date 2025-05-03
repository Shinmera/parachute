(asdf:defsystem parachute
  :version "1.5.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
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
               :trivial-source-location
               :form-fiddle))

(asdf:defsystem parachute/example
  :components ((:file "example"))
  :depends-on (:parachute)
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.parachute.example)))

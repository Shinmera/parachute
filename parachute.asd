(asdf:defsystem parachute
  :version "1.5.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An extensible and cross-compatible testing framework."
  :homepage "https://shinmera.com/docs/parachute/"
  :bug-tracker "https://shinmera.com/project/parachute/issues"
  :source-control (:git "https://shinmera.com/project/parachute.git")
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

(asdf:defsystem parachute/example
  :components ((:file "example"))
  :depends-on (:parachute)
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.parachute.example)))

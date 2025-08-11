(asdf:defsystem parachute-lisp-unit
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Parachute's lisp-unit compatibility layer."
  :homepage "https://shinmera.com/docs/parachute/"
  :bug-tracker "https://shinmera.com/project/parachute/issues"
  :source-control (:git "https://shinmera.com/project/parachute.git")
  :serial T
  :components ((:file "lisp-unit"))
  :depends-on (:parachute))

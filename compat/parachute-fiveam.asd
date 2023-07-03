(asdf:defsystem parachute-fiveam
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Parachute's FiveAM compatibility layer."
  :homepage "https://Shinmera.github.io/parachute/"
  :bug-tracker "https://github.com/Shinmera/parachute/issues"
  :source-control (:git "https://github.com/Shinmera/parachute.git")
  :serial T
  :components ((:file "fiveam"))
  :depends-on (:parachute))

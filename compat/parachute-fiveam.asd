#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem parachute-fiveam
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Parachute's FiveAM compatibility layer."
  :homepage "https://github.com/Shinmera/parachute"
  :serial T
  :components ((:file "fiveam"))
  :depends-on (:parachute))

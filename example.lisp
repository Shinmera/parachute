#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

(define-test dependency
  (false (sleep 1)))

(define-test example
  :depends-on (dependency)
  (true T)
  (false NIL)
  (is < 5 8)
  (of-type symbol 'foo))

(define-test subtest
  :parent example
  (fail (error "An expected failure.")))

(define-test failing
  (is < 8 5))

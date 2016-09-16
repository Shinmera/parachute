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
  (true (< 8 5))
  (fail 'happy)
  (true (= 5 (read-from-string "OH-SHIT"))))

(define-test partially-correct
  (true "Happy")
  (is = (get-universal-time) 2962566000))

(define-test shuffle
  :serial NIL
  (true 1)
  (true 2)
  (true 3)
  (true 4)
  (true 5))

(define-test timeout
  :time-limit 0.5
  (sleep 0.75))

(define-test bad-dependency
  :depends-on (failing)
  (is = 5 5))


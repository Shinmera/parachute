#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:parachute
  (:nicknames #:org.shirakumo.parachute)
  (:use #:cl)
  ;; fixture.lisp
  (:export
   #:*fixture-captures*
   #:*fixture-restores*
   #:define-fixture-capture
   #:define-fixture-restore
   #:package-fixtures
   #:capture-fixtures
   #:restore-fixtures
   #:call-with-fixtures
   #:with-fixtures)
  ;; report.lisp
  (:export
   #:test
   #:report
   #:tests-with-status
   #:summarize
   #:quiet
   #:plain
   #:output
   #:report-on
   #:interactive)
  ;; result.lisp
  (:export
   #:*parent*
   #:*context*
   #:eval-in-context
   #:result-for-testable
   #:result
   #:expression
   #:status
   #:duration
   #:description
   #:format-result
   #:value-result
   #:value
   #:multiple-value-result
   #:finishing-result
   #:comparison-result
   #:value-form
   #:expected
   #:comparison
   #:comparison-geq
   #:multiple-value-comparison-result
   #:parent-result
   #:results
   #:find-child-result
   #:results-with-status
   #:test-result)
  ;; test.lisp
  (:export
   #:test
   #:name
   #:home
   #:parent
   #:children
   #:referenced-dependencies
   #:dependencies
   #:fixtures
   #:time-limit
   #:referenced-skips
   #:skipped-children
   #:tests
   #:serial
   #:find-test
   #:remove-test
   #:define-test
   #:package-tests)
  ;; tester.lisp
  (:export
   #:true
   #:false
   #:is
   #:isnt
   #:is-values
   #:isnt-values
   #:fail
   #:of-type
   #:finish
   #:with-forced-status
   #:skip)
  ;; toolkit.lisp
  (:export
   #:with-shuffling))

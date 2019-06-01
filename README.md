## About parachute
Parachute is a simple-to-use and extensible testing framework.

## How To
In Parachute, things are organised as a bunch of named tests within a package. Each test can contain a bunch of test forms that make up its body. By default `true`, `false`, `fail`, `is`, `isnt`, `is-values`, `isnt-values`, `of-type`, and `finish` are provided as test forms.

    (define-test numbers
      (of-type integer 5)
      (true (numberp 2/3))
      (false (numberp :keyword))
      (is-values (values 0 "1")
        (= 0)
        (equal "1")))

The name of a test can be either a symbol or a string, though both get coerced to a string internally.

    (define-test "A more descriptive name, perhaps")

Tests can have dependencies in order to ensure that you don't get cascade failures. If a test's dependency fails, it is automatically skipped.

    (define-test equality
      :depends-on (numbers)
      (is = 0 1))             ; Bogus

    (define-test arithmetic
      :depends-on (equality)
      (is = 5 (+ 2 3)))

Dependencies can also be logically combined if you require more complicated dependency logic.

    (define-test unexpected-failure-backup-test
      :depends-on (:and equality (:not arithmetic))
      (of-type number (+ 2 3))) ; Maybe everything broke?

Often times it also makes sense to organise tests according to a hierarchy. For example you could have tests that reflect your type hierarchy, or other kind of structure within your system. You can also use this to create simple test suites.

    (define-test suite)

    (define-test test-a
      :parent suite
      #|...|#)

    (define-test (suite test-b)
      #|...|#)

    (define-test other-suite
      #|...|#
      (define-test other-test
         #|...|#))

Sometimes it is then useful to skip children if you know that they are either faulty or incomplete and shouldn't yet be tested as part of the greater scheme.

    (define-test suite
      :skip (test-a))

If you need to skip individual test forms rather than a whole test, you can use the `skip` form.

    (define-test stuff
      (true :pass)
      (skip "Not ready yet"
        (is = 5 (some-unimplemented-function 10))))

If you need to skip tests depending on the implementation, or the presence of other feature combinations, you can use `skip-on`.

    (define-test stuff
      (skip-on (clisp) "Not supported on clisp."
        (is equal #p"a/b/" (merge-pathnames "b/" "a/"))))

In order to ensure that there is no accidental sequential dependency between test forms or children, you can use the `:serial NIL` option, which will shuffle the test forms and children each time before evaluating them.

    (define-test random
      :serial NIL
      (true 1)
      (true 2)
      (true 3)
      (true 4)
      (true 5))

If you need to wrap your test forms in some kind of environment, then the shuffling won't work automatically. However, you can fix this by wrapping the forms in a `with-shuffling` form.

    (define-test random-2
      (let ((a 0))
        (with-shuffling
          (is = 1 (incf a))
          (is = 2 (incf a))
          (is = 3 (incf a)))))

In case your code will cause changes to the global environment, you probably will want to fix it in place to make sure they are restored to their former values after the test completes. Parachute allows you to automatically fix variables, functions, and macros in place.

    (define-test float-format
      :fix (*read-default-float-format*)
      (of-type single-float (read-from-string "0.5"))
      (setf *read-default-float-format* 'double-float)
      (of-type double-float (read-from-string "0.5")))

You can also tell it to hold all the symbols accessible to a certain package in place by giving it a package designator as a keyword, gensym, or string. Using `with-fixtures`, this can also be done locally. It expects an evaluated list of fixtures.

    (define-test float-format2
      (with-fixtures '(*read-default-float-format*)
        (setf *read-default-float-format* 'double-float)
        (of-type double-float (read-from-string "0.5")))
      (of-type single-float (read-from-string "0.5")))

If you have a user-defined binding to a symbol you can also make the fixture system aware of it so that it'll capture the bindings automatically.

    (define-fixture-capture my-binding (symbol)
      (when (my-binding-bound-p symbol)
        (values (my-binding-value symbol) T)))

    (define-fixture-restore my-binding (symbol value)
      (setf (my-binding-value symbol) value))

Sometimes the compiler will already complain for some tests that you expect to fail, for instance when the type inference is too good. In that case you can force Parachute to only compile the test forms when the test is evaluated. This is also useful when you're continuously working on macros and the like and don't want to recompile the test all the time. Parachute does not do this by default in order to give you useful compiler feedback in case you mess up writing your tests, and in order to avoid having to pay the time to invoke the compiler when it usually isn't necessary. Note that the compilation of the forms will be factored into your timing results.

    (define-test compare-numbers
      :compile-at :execute
      (fail (= :5 5)))

Finally, Parachute also allows you to enforce timing constraints on tests to make sure that they complete within a specified limit.

    (define-test too-slow
      :time-limit 0.5
      (sleep 1))

Tests are run under a `report` context, which gathers a bunch of `result` objects that are generated as part of the evaluation and is responsible for presenting them to the user in some hopefully useful manner. The standard report is `plain`, which reports things as text to the REPL.

    (test 'arithmetic)

![plain-test-result](https://filebox.tymoon.eu/file/TVRFMU5RPT0=)

There are two more report types included in the standard distribution, namely `quiet` and `interactive`. The former should not produce any output or user-interaction whatseover.

    (test 'arithmethic :report 'quiet)

![quiet-test-result](https://filebox.tymoon.eu/file/TVRFMU5BPT0=)

The latter on the other hand will present you the debugger with a handful of useful restarts whenever a test fails. This allows you to iteratively and dynamically develop and refine tests.

    (test 'arithmetic :report 'interactive)

![interactive-test-result](https://filebox.tymoon.eu/file/TVRFMU5nPT0=)

And that should cover most of it. Parachute does not have any fancy ASDF integration, however I don't believe any is needed anyway. Simply modifying your main system and test system as follows should be sufficient.

    (asdf:defsystem main-system
      #|...|#
      :in-order-to ((asdf:test-op (asdf:test-op :test-system))))

    (asdf:defsystem test-system
      #|...|#
      :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :test-package)))

This should allow you to run the tests via ASDF like so: `(asdf:test-system :main-system)`.

## Compatibility Layers
Parachute offers "zero-cost switching" by implementing basic compatibility layers that simulate another test framework's syntax and behaviour. This allows you to switch over from another test framework hopefully without having to change a single test in your suite.

Usually the only thing you need to do to get this done is change the name of the framework in your test system dependencies to one prefixed with `parachute-`. However, since there's so many test frameworks, and each layer requires quite a bit of work and investigation, only some are supported.

The layers only mirror the public API of the test frameworks and leave out any extension mechanisms. They also do not attempt to mirror the output format or interaction method by which the other frameworks functioned, as there would be little point to mirroring them exactly.

Compatibility layers exist for:

* [5am](https://common-lisp.net/project/fiveam/)
* [lisp-unit](https://github.com/OdonataResearchLLC/lisp-unit)
* [prove](https://github.com/fukamachi/prove)  
  Note: make sure to both change the `:depends-on` and `:defsystem-depends-on` to `parachute-prove` if you happen to use the ASDF extension.

## Extending Parachute
### Test and Result Evaluation
Parachute follows its own evaluation semantics in order to run tests. Primarily this means that most everything goes through one central function called `eval-in-context`. This functions allows you to customise evaluation based on both what the context is, and what the object being "evaluated" is.

Usually the context is a report object, but other situations might also be conceived. Either way, it is your responsibility to add methods to this function when you add a new `result` type, some kind of `test` subclass, or a new `report` type that you want to customise according to your desired behaviour.

The evaluation of results is decoupled from the context and reports in the sense that their behaviour does not, by default, depend on it. At the most basic, the `result` class defines a single `:around` method that takes care of recording the `duration` of the test evaluation, setting a default `status` after finishing without errors, and skipping evaluation if the status is already set to something other than `:unknown`.

Next we have a result object that is interesting for anything that actually produces direct test results-- `value-result`. Upon evaluation, if the `value` slot is not yet bound, it calls its `body` function and stores the return value thereof in the `value` slot.

However, the result type that is actually used for all standard test forms is the `comparison-result`. This also takes a comparator function and an expected result to compare against upon completion of the test. If the results match, then the test status is set to `:passed`, otherwise to `:failed`.

Since Parachute allows for a hierarchy in your tests, there have to be aggregate results as well, and indeed there are. Two of them, actually. First is the base case, namely `parent-result` which does two things on evaluation: one, it binds `*parent*` to itself to allow other results to register themselves upon construction, and two it sets its status to `:failed` if any of the `children` have failed.

Finally we have the `test-result` which takes care of properly evaluating an actual `test` object. What this means is to evaluate all `dependencies` before anything else happens, and to check the time limit after everything else has happened. If the time limit has exceeded, set the `description` accordingly and mark the result as `:failed`. For its main `eval-in-context` method however it checks whether any of the dependencies have failed, and if so, mark itself as `:skipped`. Otherwise it calls `eval-in-context` on the actual test object.

The default evaluation procedure for a test itself is to simply call all the functions in the `tests` list in a `with-fixtures` environment.

And that describes the semantics of default test procedures. Actual test forms like `is` are created through macros that emit an `(eval-in-context *context* (make-instance 'comparison-result #|...|#))` form. The `*context*` object is automatically bound to the context object on call of `eval-in-context` and thus always refers to the current context object. This allows results to be evaluated even from within opaque parts like user-defined functions.

### Report Generation
Finally we come to the question of how to generate a report and interact with the evaluation process. The most primitive idea for a report is one that doesn't do anything at all, except for perhaps catching stray errors. This is implemented by the `quiet` report object, which only has a single `eval-in-context` `:around` method that has a handler-case around the rest.

It should be possible to get any kind of reporting behaviour you want by adding methods that specialise on your report object to `eval-in-context`. For the simple case where you want something that prints to the REPL but has a different style than the preset `plain` report, you can simply subclass that and specialise on the `report-on` and `summarize` functions that then produce the output you want.

Since you can control pretty much every aspect of evaluation rather closely, very different behaviours and recovery mechanisms are also possible to achieve. One final aspect to note is `result-for-testable`, which should return an appropriate result object for the given testable. This should only return fresh result objects if no result is already known for the testable in the given context. The standard tests provide for this, however they only ever return a standard `test-result` instance. If you need to customise the behaviour of the evaluation for that part, it would be a wise idea to subclass `test-result` and make sure to return instances thereof from `result-for-testable` for your report.

Finally it should be noted that if you happen to create new `result` types that you might want to run using the default reports, you should add methods to `format-result` that specialise on the keywords `:oneline` and `:extensive` for the type. These should return a string containing an appropriate description of the test in one line or extensively, respectively. This will allow you to customise how things look to some degree without having to create a new report object entirely.

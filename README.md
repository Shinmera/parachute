## About parachute
Parachute is a simple-to-use and extensible testing framework.

## How To
In Parachute, things are organised as a bunch of named tests within a package. Each test can contain a bunch of test forms that make up its body. By default `true`, `false`, `fail`, `is`, and `of-type` are provided as test forms.

    (define-test numbers
      (of-type integer 5)
      (true (numberp 2/3))
      (false (numberp :keyword)))

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
      ...)

    (define-test (suite test-b)
      ...)

    (define-test other-suite
      ...
      (define-test other-test
         ...))

Sometimes it is then useful to skip children if you know that they are either faulty or incomplete and shouldn't yet be tested as part of the greater scheme.

    (define-test suite
      :skip (test-a))

In order to ensure that there is no accidental sequential dependency between test forms or children, you can use the `:serial NIL` option, which will shuffle the test forms and children each time before evaluating them.

    (define-test random
      :serial NIL
      (true 1)
      (true 2)
      (true 3)
      (true 4)
      (true 5))

In case your code will cause changes to the global environment, you probably will want to fix it in place to make sure they are restored to their former values after the test completes. Parachute allows you to automatically fix variables, functions, and macros in place.

    (define-test float-format
      :fix (*read-default-float-format*)
      (of-type single-float 0.5)
      (setf *read-default-float-format* 'double-float)
      (of-type double-float 0.5))

You can also tell it to hold all the symbols accessible to a certain package in place by giving it a package designator as a keyword, gensym, or string. If you have a user-defined binding to a symbol you can also make the fixture system aware of it.

    (define-fixture-capture my-binding (symbol)
      (when (my-binding-bound-p symbol)
        (values (my-binding-value symbol) T)))

    (define-fixture-restore my-binding (symbol value)
      (setf (my-binding-value symbol) value))

Finally, Parachute also allows you to enforce timing constraints on tests to make sure that they complete within a specified limit.

    (define-test too-slow
      :time-limit 0.5
      (sleep 1))

Tests are run under a `report` context, which gathers a bunch of `result` objects that are generated as part of the evaluation and is responsible for presenting them to the user in some hopefully useful manner. The standard report is `plain`, which reports things as text to the REPL.

    (test 'arithmetic)

![plain-test-result](https://filebox.tymoon.eu/file/TVRFMU1RPT0=)

There are two more report types included in the standard distribution, namely `quiet` and `interactive`. The former should not produce any output or user-interaction whatseover.

    (test 'arithmethic :report 'quiet)

![quiet-test-result](https://filebox.tymoon.eu/file/TVRFMU1nPT0=)

The latter on the other hand will present you the debugger with a handful of useful restarts whenever a test fails. This allows you to iteratively and dynamically develop and refine tests.

    (test 'arithmetic :report 'interactive)

![interactive-test-result](https://filebox.tymoon.eu/file/TVRFMU13PT0=)

And that should cover most of it. Parachute does not have any fancy ASDF integration, however I don't believe any is needed anyway. Simply modifying your main system and test system as follows should be sufficient.

    (asdf:defsystem main-system
      ...
      :in-order-to ((asdf:test-op (asdf:test-op :test-system))))

    (asdf:defsystem test-system
      ...
      :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :test-system)))

This should allow you to run the tests via ASDF like so: `(asdf:test-system :main-system)`.

## Compatibility Layers
Parachute offers "zero-cost switching" by implementing basic compatibility layers that simulate another test frameworks' syntax and behaviour. This allows you to switch over from another test framework hopefully without having to change a single test in your suite.

Usually the only thing you need to do to get this done is change the name of the framework in your test system dependencies to one prefixed with `parachute-`. However, since there's so many test frameworks, and each layer requires quite a bit of work and investigation, only some are supported.

The layers only mirror the public API of the test frameworks and leave out any extension mechanisms. They also do not attempt to mirror the output format or interaction method by which the other frameworks functioned, as there would be little point to mirroring them exactly.

Portability layers exist for:

* 5am
* lisp-unit
* Prove

## Extending Parachute
### Test and Result Evaluation
Parachute follows its own evaluation semantics in order to run tests. Primarily this means that most everything goes through one central function called `eval-in-context`. This functions allows you to customise evaluation based on both what the context is, and what the object being "evaluated" is.

Usually the context is a report object, but other situations might also be conceived. Either way, it is your responsibility to add methods to this function when you add a new `result` type, some kind of `test` subclass, or a new `report` type that you want to customise according to your desired behaviour.

The evaluation of results is decoupled from the context and reports in the sense that their behaviour does not, by default, depend on it. At the most basic, the `result` class defines a single `:around` method that takes care of recording the `duration` of the test evaluation, setting a default `status` after finishing without errors, and skipping evaluation if the status is already set to something other than `:unknown`.

Next we have a result object that is interesting for anything that actually produces direct test results-- `value-result`. If the `value` slot thereof is set to a `function` object, the function is called upon evaluation of the result, and the value slot's value is then replaced with whatever the function call returns.

However, the result type that is actually used for all standard test forms is the `comparison-result`. This also takes a comparator function and an expected result to compare against upon completion of the test. If the results match, then the test status is set to `:passed`, otherwise to `:failed`.

Since Parachute allows for a hierarchy in your tests, there have to be aggregate results as well, and indeed there are. Two of them, actually. First is the base case, namely `parent-result` which does two things on evaluation: one, it binds `*parent*` to itself to allow other results to register themselves upon construction, and two it sets its status to `:failed` if any of the `children` have failed.

Finally we have the `test-result` which takes care of properly evaluating an actual `test` object. What this means is to evaluate all `dependencies` before anything else happens, and to check the time limit after everything else has happened. If the time limit has exceeded, set the `description` accordingly and mark the result as `:failed`. For its main `eval-in-context` method however it checks whether any of the dependencies have failed, and if so, mark itself as `:skipped`. Otherwise it calls `eval-in-context` on the actual test object. Finally it evaluates every one of the `children` of the test, making sure to mark the ones in the `skipped-children` list as `:skipped` beforehand.

The default evaluation procedure for a test itself is to simply call all the functions in the `tests` list in a `with-fixtures` environment.

And that describes the semantics of default test procedures. Actual test forms like `is` are created through macros that emit an `(eval-in-context *context* (make-instance 'comparison-result ...))` form. The `*context*` object is automatically bound to the context object on call of `eval-in-context` and thus always refers to the current context object. This allows results to be evaluated even from within opaque parts like user-defined functions.

### Report Generation
Finally we come to the question of how to generate a report and interact with the evaluation process. The most primitive idea for a report is one that doesn't do anything at all, except for perhaps catching stray errors. This is implemented by the `quiet` report object, which only has a single `eval-in-context` `:around` method that has a handler-case around the rest.

It should be possible to get any kind of reporting behaviour you want by adding methods that specialise on your report object to `eval-in-context`. For the simple case where you want something that prints to the REPL but has a different style than the preset `plain` report, you can simply subclass that and specialise on the `report-on` and `summarize` functions that then produce the output you want.

Since you can control pretty much every aspect of evaluation rather closely, very different behaviours and recovery mechanisms are also possible to achieve. One final aspect to note is `result-for-testable`, which should return an appropriate result object for the given testable. This should only return fresh result objects if no result is already known for the testable in the given context. The standard tests provide for this, however they only ever return a standard `test-result` instance. If you need to customise the behaviour of the evaluation for that part, it would be a wise idea to subclass `test-result` and make sure to return instances thereof from `result-for-testable` for your report.

Finally it should be noted that if you happen to create new `result` types that you might want to run using the default reports, you should add methods to `format-result` that specialise on the keywords `:oneline` and `:extensive` for the type. These should return a string containing an appropriate description of the test in one line or extensively, respectively. This will allow you to customise how things look to some degree without having to create a new report object entirely.

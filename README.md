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

There are two more report types included in the standard distribution, namely `quiet` and `interactive`. The former should not produce any output or user-interaction whatseover.

    (test 'arithmethic :report 'quiet)

The latter on the other hand will present you the debugger with a handful of useful restarts whenever a test fails. This allows you to iteratively and dynamically develop and refine tests.

    (test 'arithmetic :report 'interactive)

And that should cover most of it. Parachute does not have any fancy ASDF integration, however I don't believe any is needed anyway. Simply modifying your main system and test system as follows should be sufficient.

    (asdf:defsystem main-system
      ...
      :in-order-to ((asdf:test-system (asdf:test-system :test-system))))

    (asdf:defsystem test-system
      ...
      :in-order-to ((asdf:test-system (funcall (find-symbol (string :test) :parachute) 
                                               :test-package))))

This should allow you to run the tests via ASDF like so: `(asdf:test-system :main-system)`.

## Extending Parachute
FIXME: time limits
FIXME: documentation parts in test result presentation
FIXME: confusing output on comparison-results
FIXME: some results seem to be off, investigate
FIXME: finish this readme

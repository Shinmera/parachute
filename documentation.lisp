#|
 This file is a part of parachute
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parachute)

;; fixture.lisp
(docs:define-docs
  (variable *fixture-captures*
    "A map of names to fixture capture functions.

See DEFINE-FIXTURE-CAPTURE")

  (variable *fixture-restores*
    "A map of names to fixture restore functions.

See DEFINE-FIXTURE-RESTORE")

  (function define-fixture-capture
    "Defines a capture function to fix a symbol binding.

A capture function should take a symbol and return a value to
capture for it, and T as the secondary value if anything should
be captured at all for the kind of binding the function handles.

See *FIXTURE-CAPTURES*")

  (function define-fixture-restore
    "Defines a restore function to fix a symbol binding.

A restoration function should take a symbol and a value and
make sure to restore the binding it handles to the given value.

See *FIXTURE-RESTORES*")

  (function package-fixtures
    "Returns a list of fixtures for all symbols accessible in the given package.")

  (function capture-fixtures
    "Returns a list of fixtures for the given kind.

If the given name is a symbol that is not a keyword and has a
package associated with it, then a list of a single fixture that
captures every appropriate binding for the symbol is returned.
Otherwise the name is interpreted as a package for which a list
of fixtures for every symbol accessible in the package is returned.

See RESTORE-FIXTURES
See PACKAGE-FIXTURES
See DEFINE-FIXTURE-CAPTURE")

  (function restore-fixtures
    "Restores the bindings stored in the fixtures to their saved values.

See CAPTURE-FIXTURES
See DEFINE-FIXTURE-RESTORE")

  (function call-with-fixtures
    "Calls the function with the given list of fixtures.

This ensures that the values are stored before the function is
called and restored once the function is completed.

See CAPTURE-FIXTURES
See RESTORE-FIXTURES
See WITH-FIXTURE")

  (function with-fixtures
    "Shorthand macro to evaluate the body with the fixtures saved away.

See CALL-WITH-FIXTURES"))

;; report.lisp
(docs:define-docs
  (function resolve-tests
    "Resolves the given test designator into a list of tests.

The designator can be of type
  LIST    -- RESOLVE-TESTS is called on each element of the list
             and the results are concatenated together.
  PACKAGE -- Returns a list of all tests in the given package.
  SYMBOL
  STRING  -- If the name resolves to a test by FIND-TEST, a list
             only containing that test is returned. If the name
             resolves to a package by FIND-PACKAGE, a list of all
             tests in the package is returned. If neither can be
             found, an error is signalled.

See FIND-TEST
See PACKAGE-TESTS")

  (function test
    "Run a test.

DESIGNATOR is resolved to a list of tests to run under a single
report. You can specify the report class by the REPORT keyword
argument. All other arguments are used as initargs for the REPORT
creation.

After the report has been created, each test is run via 
EVAL-IN-CONTEXT. Finally SUMMARIZE is called on the report.

See REPORT
See RESOLVE-TESTS
See EVAL-IN-CONTEXT
See SUMMARIZE")

  (function summarize
    "Should cause the report to produce some kind of summary.

How this summary is presented and what is contained in it is up to
the report itself.

See REPORT.")

  (type report
    "Base class for all report objects.

Reports are responsible for gathering and representing the
results of a test evaluation. The manner in which the results are
presented to the user is up to the test in question.

See PARENT-RESULT
See SUMMARIZE")

  (function tests-with-status
    "Returns a list of TEST objects that have been evaluated and whose reports have the requested status.

See REPORT
See RESULTS-WITH-STATUS")

  (type quiet
    "A quiet report that doesn't report nor do anything special.

Simply returns the report object on SUMMARIZE.

See REPORT")

  (type plain
    "A plain-text report that prints test results as they go by as well as a summary of the failures at the end.

See REPORT
See OUTPUT
See REPORT-ON")

  (function output
    "The stream that the PLAIN report outputs to during REPORT-ON and SUMMARIZE.

See PLAIN
See REPORT-ON
See SUMMARIZE")

  (function report-on
    "Causes the result to be printed to standard output under the formatting of the report.

See REPORT
See RESULT")

  (type interactive
    "An interactive test report that shows the debugger on every failure, with restarts that allow you to decide what to do next.

See REPORT"))

;; result.lisp
(docs:define-docs
  (variable *parent*
    "Variable storing the immediate parent in the current evaluation context.

This is used in order to register a result within its parent.")
  
  (variable *context*
    "Variable storing the current context of the evaluation.

While EVAL-IN-CONTEXT already carries the context as an argument,
in certain situations like when opaque functions are evaluated, testers
within the execution context must be able to access the context object
in order to evaluate properly.")

  (function eval-in-context
    "Evaluates the thing within the given context.

This may seem like a rather generic description. In specific, this is mostly
used in order to provide a customizable evaluation behaviour during the
evaluation of tests under a given report. Which is to say that usually the 
context will be some report object and the thing being tested some kind of
result or test instance.

See REPORT
See RESULT")

  (function result-for-testable
    "Returns an appropriate RESULT instance for the given testable and context.

You should specialize this if you need special kinds of result types for your
report.

See RESULT")

  (type result
    "Base container object for test results of any kind.

A result always has:
  EXPRESSION  -- An expression describing what this result is about.
  STATUS      -- One of :UNKNOWN :FAILED :PASSED :SKIPPED.
  DURATION    -- The duration (in seconds) the evaluation took, or NIL.
  DESCRIPTION -- A string describing the result, or NIL.

See EXPRESSION
See STATUS
See DURATION
See DESCRIPTION")

  (function expression
    "The expression that the result object is about.

See RESULT")

  (function status
    "The status of the result object.

Should be one of :UNKNOWN :FAILED :PASSED :SKIPPED. By default after 
initialization the status is :UNKNOWN. The status is automatically 
changed to :PASSED if the result is still :UNKNOWN after EVAL-IN-CONTEXT
 completes.

See RESULT")

  (function duration
    "The duration the evaluation of the result object took in seconds or NIL.

This is automatically recorded in standard methods for EVAL-IN-CONTEXT.
The value may be NIL if the test has not yet been evaluated at all.

See RESULT")

  (function description
    "The description of what the test/result is about as a string or NIL.

See RESULT
See TEST")

  (function format-result
    "Formats the result into the requested representation as a string.

The representation type can, by default, be either :ONELINE or :EXTENSIVE.

See RESULT")

  (type value-result
    "A result that carries some kind of test result value.

Typically the value slot is set to a function object, which is then
called on EVAL-IN-CONTEXT and whose return value is then stored in the
value slot in place of the function.

See VALUE
See RESULT")

  (function value
    "The value of the result object.

Note that it is not possible to determine whether the value-result object
has been run based on this value. It may be a function object, which will
be interpreted as having to be called on EVAL-IN-CONTEXT.

See VALUE-RESULT")

  (type finishing-result
    "A result that only passes if its evaluation proceeds without escaping.

See VALUE-RESULT")

  (type comparison-result
    "A result that compares against an expected value by some kind of comparator.

This result sets its status based on the boolean return value of a test
of its value against an expected, preset value.

See VALUE-FORM
See EXPECTED
See COMPARISON
See VALUE-RESULT")

  (function value-form
    "A literal representation of the form that produces the value for the test.

See COMPARISON-RESULT")

  (function expected
    "A value that should be equal (under some predicate) to what the test evaluates to.

See COMPARISON-RESULT")

  (function comparison
    "The comparison function designator that compares the expected and actual values of the test.

See COMPARISON-RESULT")

  (type parent-result
    "A result that does not directly perform a test, but rather serves as an aggregate for multiple tests.

This result will set its status to :FAILED automatically if it notices that
after evaluation one or more of its children have the status :FAILED.

See RESULT
See FIND-CHILD-RESULT
See RESULTS-WITH-STATUS")

  (function find-child-result
    "Attempts to find the result object associated with the given test.

This simply tests by EQ against the EXPRESSION of each child within the result.

See PARENT-RESULT")

  (function results-with-status
    "Returns a list of results that are a child of the result and have the requested status.

See PARENT-RESULT")

  (type test-result
    "A result object for tests.

This takes care of properly evaluating the test by following these steps:
1. Run all tests in the test's DEPENDENCIES as per EVAL-DEPENDENCY-COMBINATION
2. Check that all dependencies have a :PASSED status as per the dependency
   combination by running CHECK-DEPENDENCY-COMBINATION
3. For each child in the test's CHILDREN
   create a result by RESULT-FOR-TESTABLE
   if the child is part of the test's SKIPPED-CHILDREN
   mark its result as :SKIPPED and EVAL-IN-CONTEXT it.
   Otherwise EVAL-IN-CONTEXT the child's result directly.

The methods implemented by PARENT-RESULT and RESULT will in turn
take care to implement proper status setting and avoiding duplicate
evaluation.

See PARENT-RESULT
See EVAL-DEPENDENCY-COMBINATION
See CHECK-DEPENDENCY-COMBINATION"))

;; test.lisp
(docs:define-docs
  (variable *test-indexes*
    "A hash-table from packages to hash-tables from strings to test instances.

Contains the maps of packages to tests.")

  (type test
    "Container for test collections.

Manages all the data that ties different collections together
and manages the various options you might want to have for tests.

Note that merely initializing a test instance will not register it
to be found by FIND-TEST. It will also not tie it in with the rest
of the tests. An error will be signalled, if a parent is designated
that does not exist, and warnings will be signalled if dependencies
are designated that do not exist.

In order for the test to be tied in and registered to be findable,
use (SETF (FIND-TEST name home) test). In order to remove it, simply
use REMOVE-TEST.

See NAME
See HOME
See DESCRIPTION
See PARENT
See CHILDREN
See DEPENDENCIES
See REFERENCED-DEPENDENCIES
See FIXTURES
See TIME-LIMIT
See SKIPPED-CHILDREN
See REFERENCED-SKIPS
See TESTS
See SERIAL
See FIND-TEST
See REMOVE-TEST
See DEFINE-TEST")

  (function name
    "The name of the test, which is always a string.

See TEST")

  (function home
    "The home package of the test.

See TEST")

  (function parent
    "The parent test object, if any.

If a test has a parent, then it must also itself be contained in the parent's
children list.

See TEST")

  (function children
    "The list of child test instances.

Note that the return value of this will be shuffled randomly if the test is
marked as non-serial. Child tests will be evaluated after the test itself.

See SERIAL
See TEST")

  (function referenced-dependencies
    "The logical combination of test designators that reference a dependant test.

DEPENDENCIES ::= (DEPENDENCY*) | (LOGOP DEPENDENCY*)
LOGOP        ::= :OR | :AND | :NOT
DEPENDENCY   ::= test-name | (home test-name) | DEPENDENCIES

See DEPENDENCIES
See TEST")

  (function dependencies
    "The logical combination of dependant tests.

DEPENDENCIES ::= (LOGOP DEPENDENCY*)
LOGOP        ::= :OR | :AND | :NOT
DEPENDENCY   ::= test-object | DEPENDENCIES

If a dependency is referenced that does not exist, an error is signalled.
Dependant tests must be evaluated before the test itself.

See REFERENCED-DEPENDENCIES
See CHECK-DEPENDENCY-COMBINATION
See EVAL-DEPENDENCY-COMBINATION
See RESOLVE-DEPENDENCY-COMBINATION
See TEST")

  (function fixtures
    "The list of fixed symbols or packages.

See WITH-FIXTURE
See TEST")

  (function time-limit
    "The time limit of the test, if any.

In order for the test to succeed, the evaluation must succeed before
the time limit is reached. Otherwise the test is marked as having failed.

See TEST")

  (function referenced-skips
    "The list of test designators that reference a skipped test.

See SKIPPED-CHILDREN
See TEST")

  (function skipped-children
    "The list of the children that should be skipped.

If a child is referenced that does not exist, an error is signalled.
If a child is within this list, it will not be evaluated after the test
and instead be marked as skipped automatically.

See REFERENCED-SKIPS
See TEST")

  (function tests
    "The list of functions to execute in order to actually perform the tests.

Note that the return value of this will be shuffled randomly if the test is
marked as non-serial.

See SERIAL
See TEST")

  (function serial
    "Whether the tests and children should be evaluated in order, or randomly.

This affects the return values of TESTS and CHILDREN.

See CHILDREN
See TESTS
See TEST")

  (function test-index
    "Returns the map for names to test instances.

See FIND-TEST")

  (function find-test
    "Finds the test by name if possible.

If the name is a symbol, the package defaults to the symbol's package.
If the name is a string, the package defaults to *PACKAGE*.

This function can also be used as a place in order to register a test.
When a test is set, the preexisting test if any is first removed by
REMOVE-TEST. This ensures that potentially removed options need to be
properly updated within other affected tests. After that, the test is
added to the list of children within its parent, if one is set. Finally
the test is registered within the test index.

See REMOVE-TEST
See *TEST-INDEX*")

  (function remove-test
    "Removes the test by name if possible.

When a test is removed, it is also automatically removed from its
parent's child list. However, weak references such as within a test's
dependencies cannot be updated as there is no backreference.

See FIND-TEST
See *TEST-INDEX*")

  (function define-test
    "Defines a test.

NAME can be either a string, a symbol, or a list of a parent name and a name.

BODY ::= KARG* form*
KARG ::= keyword expression

Each karg in the body is used as an initialization argument to the test
instance with the exception of TEST-CLASS, which is used as the class-name
to create a test instance with and defaults to TEST. The values of the
keyword arguments are always passed quoted.

If you set a parent in the NAME, you cannot use the :PARENT karg and vice-
versa. The following initargs are automatically always passed by DEFINE-TEST
and should thus not appear as a karg: :NAME :TESTS :PARENT

You can define child tests within the body of the test if they appear as
\"toplevel\" forms. They will be taken out of the body forms and emitted
as definition forms after the parent form, with the parent automatically set
appropriately. As such, subtests defined in this manner cannot cause any
effects on the surrounding environment.

Each form within the body will be turned into a separate test function.
This is done so that the tests can be shuffled randomly if the test is not
set as serial. This means that if you need to wrap multiple testers in any
other form, the inner tests cannot be shuffled accordingly. However, you can
use the WITH-SHUFFLING macro.

The karg :COMPILE-AT directs whether each test form should be compiled at
definition time (default, value :COMPILE-TIME) or at test evaluation time
(value :EXECUTE). The latter is achieved by, instead of a straight-up LAMBDA
for each test form, it wraps it in a LAMBDA that calls COMPILE on the source
form.

Note that the dependencies of the test can be a combined by logic operators
of :AND :OR and :NOT. See REFERENCED-DEPENDENCIES for the necessary structure.

See TEST
See FIND-TEST
See REMOVE-TEST
See REFERENCED-DEPENDENCIES")

  (function package-tests
    "Returns a list of all the tests defined in the given package.

Signals an error if the PACKAGE cannot be resolved to a package object.

See *TEST-INDEXES*")

  (function resolve-dependency-combination
    "Resolves the dependency combination COMBINATION for the TEST object.

This turns all referenced test in the structure to actual test instances.

COMBINATION ::= (LOGOP DEPENDENCY*)
LOGOP       ::= :OR | :AND | :NOT
DEPENDENCY  ::= test-name | (home test-name) | DEPENDENCY

If a dependency that does not exist is referenced or the above structure is
violated, an error is signalled.

See REFERENCED-DEPENDENCIES
See FIND-TEST")

  (function eval-dependency-combination
    "Evaluates the dependency combination under the given context.

This simply traverses the structure recursively and calls EVAL-IN-CONTEXT
on each test object. 

An error is signalled if the structure is violated.

See RESOLVE-DEPENDENCY-COMBINATION
See EVAL-IN-CONTEXT")

  (function check-dependency-combination
    "Checks each test in the dependency combination against the given status.

This traverses the dependency combination, retrieves the result from the 
context for each test, compares its STATUS against the requested one, and then
logically combines the results thereof according to the combinator's operation.

An error is signalled if the structure is violated.

See RESOLVE-DEPENDENCY-COMBINATION
See STATUS
See FIND-CHILD-RESULT"))

;; tester.lisp
(docs:define-docs
  (function true
    "A tester that succeeds if the form returns a non-NIL value.")

  (function false
    "A tester that succeeds if the form returns NIL as the value.")

  (function is
    "A tester that succeeds if the value the form returns is equal to the expected value under the given comparator.")

  (function fail
    "A tester that succeeds if the form signals an error of the requested type, defaulting to ERROR.")

  (function of-type
    "A tester that succeeds if the form returns a value that is of the requested type.")

  (function finish
    "A tester that succeeds if the form returns without escaping."))

;; toolkit.lisp
(docs:define-docs
  (function shuffle
    "Shuffles the given sequence into a new one where the elements have uniformly distributed random placement.")

  (function with-shuffling
    "Causes each form in the body to be evaluated in random order each time the with-shuffling form is evaluated.

This is done by taking each form, wrapping it in a lambda, shuffling the list of
lambdas, and then funcalling each in the shuffled order.

See SHUFFLE")

  (function removef
    "Returns a new plist with the key/value pairs of the indicators removed.")

  (function locked-package-p
    "Returns T if the given package is (assumed to be) locked and whose function bindings should thus not be changed.")

  (function print-oneline
    "Attempts to print the given object to the stream designator in one line.

This is mostly to avoid the standard printer's annoying line break behaviour
as it over-eagerly inserts line breaks and makes the output spammy, especially
for the plain report. This prints vectors (not strings) and lists on one line
for sure. Everything else might still incur line breaks, but I believe that that
is mostly outside of my control.")

  (function geq
    "Returns true if the value is of the expected general boolean.

More specifically, the following table is followed:
VALUE EXPECTED RESULT
  T      T        T
 NIL     T       NIL
 NIL    NIL       T
  T     NIL      NIL")

  (function capture-error
    "Returns the condition signalled by the form, or NIL if none was signalled.

By default only subconditions of ERROR are caught.")

  (function maybe-quote
    "Quotes the expression if it is necessary to do so.

It is considered unnecessary to quote the expression if:
  - The expression is a list that has one of the following symbols as its
    first element:
    - QUOTE
    - LAMBDA
    - FUNCTION
    - SB-INT:QUASIQUOTE
    - SI:QUASIQUOTE
  - It is constant under CONSTANTP

Implementations other than SBCL and ECL are \"out of luck\" when it comes
to quasiquoting. Generally you just shouldn't quasiquote for this anyway
since it is most likely useful for packing a value in a macro where the
lexical references could not be resolved.")

  (function maybe-unquote
    "Unquotes the expression if it is plausible to do so.

If the form is a cons, it is EVALed. This is the only way I can think of to
have any kind of hope to unquote quasiquoted expressions portably. Naturally
this will error if the quasiquote contains any form of lexical references
that are unresolvable.")

  (function call-compile
    "Compiles the form with muffled warnings and calls the resulting function."))

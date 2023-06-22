(defsystem "pkgetset.sycamore.test"
  :description "Tests for pkgetset.sycamore"
  :author "Herbert Jones"
  :license "MIT"
  :version "0.0.1"

  :class :package-inferred-system
  :pathname "tests/pkgetset.sycamore.test"

  :depends-on ("parachute"
               "pkgetset"
               "pkgetset.sycamore"

               "pkgetset.sycamore.test/all")

  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :parachute :test
                                           :pkgetset.sycamore.test)))

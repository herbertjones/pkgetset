(defsystem "pkgetset.sycamore.shasht.test"
  :description "Tests for pkgetset.sycamore.shasht"
  :author "Herbert Jones"
  :license "MIT"
  :version "0.0.1"

  :class :package-inferred-system
  :pathname "tests/pkgetset.sycamore.shasht.test"

  :depends-on ("parachute"
               "arrow-macros"
               "cl-interpol"
               "named-readtables"
               "pkgetset"
               "pkgetset.sycamore.shasht"

               "pkgetset.sycamore.shasht.test/all")
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :parachute :test
                                           :pkgetset.sycamore.shasht.test)))

(defsystem "pkgetset.sycamore.test"
  :description "Tests for pkgetset.sycamore"
  :version "0.0.1"

  :class :package-inferred-system
  :pathname "pkgetset.sycamore.test"

  :depends-on ("parachute"
               "pkgetset"
               "pkgetset.sycamore"

               "pkgetset.sycamore.test/all")

  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :parachute :test
                                           :pkgetset.sycamore.test)))

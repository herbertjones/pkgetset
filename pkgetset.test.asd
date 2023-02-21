(defsystem "pkgetset.test"
  :description "Tests for pkgetset"
  :version "0.0.1"

  :class :package-inferred-system
  :pathname "pkgetset.test"

  :depends-on ("parachute"
               "pkgetset"

               "pkgetset.test/all")

  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :parachute :test
                                           :pkgetset.test)))

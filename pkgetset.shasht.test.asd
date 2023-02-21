(defsystem "pkgetset.shasht.test"
  :name "Tests for pkgetset.shasht"
  :version "0.0.1"

  :class :package-inferred-system
  :pathname "pkgetset.shasht.test"

  :depends-on ("parachute"
               "arrow-macros"
               "cl-interpol"
               "named-readtables"
               "pkgetset"
               "pkgetset.shasht"

               "pkgetset.shasht.test/all")
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :parachute :test
                                           :pkgetset.shasht.test)))

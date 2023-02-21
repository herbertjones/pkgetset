(defsystem "pkgetset"
  :name "Data oriented programming system"
  :version "0.0.1"

  :class :package-inferred-system
  :pathname "pkgetset"

  :depends-on ("alexandria"
               "serapeum"
               "sycamore"
               "trivial-do"

               "pkgetset/all")
  :in-order-to ((test-op (test-op :pkgetset.test))))

(register-system-packages "alexandria" '(:alexandria-2))

(defsystem "pkgetset.sycamore"
  :name "Nice name"
  :version "0.0.1"

  :class :package-inferred-system
  :pathname "pkgetset.sycamore"

  :depends-on ("alexandria"
               "serapeum"
               "sycamore"
               "trivial-do"
               "pkgetset"

               "pkgetset.sycamore/all")
  :in-order-to ((test-op (test-op :pkgetset.sycamore.test))))

(register-system-packages "alexandria" '(:alexandria-2))

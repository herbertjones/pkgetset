(defsystem "pkgetset.sycamore"
  :description "Sycamore persistent backed containers for pkgetset"
  :author "Herbert Jones"
  :license "MIT"
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

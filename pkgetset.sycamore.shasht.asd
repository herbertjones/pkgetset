(defsystem "pkgetset.sycamore.shasht"
  :name "JSON functionality for pkgetset using Shasht"
  :version "0.0.1"

  :class :package-inferred-system
  :pathname "pkgetset.sycamore.shasht"

  :depends-on ("shasht"
               "pkgetset"
               "pkgetset.sycamore"
               "serapeum"
               "str"
               "sycamore"

               "pkgetset.sycamore.shasht/all")

  :in-order-to ((test-op (test-op :pkgetset.sycamore.shasht.test))))

(uiop:define-package #:pkgetset.sycamore.test/containers
  (:use)
  (:mix #:cl #:parachute)
  (:import-from #:pkgetset.sycamore.test/containers/pk.gen)
  (:import-from #:pkgetset.sycamore.test/containers/pk.str)
  (:import-from #:pkgetset.sycamore.test/containers/pk.sym)
  (:import-from #:pkgetset.sycamore.test/containers/pvec))
(cl:in-package #:pkgetset.sycamore.test/containers)

(define-test "persistent keyed"
  (test :pkgetset.sycamore.test/containers/pk.gen)
  (test :pkgetset.sycamore.test/containers/pk.str)
  (test :pkgetset.sycamore.test/containers/pk.sym))

(define-test "persistent vector"
  (test :pkgetset.sycamore.test/containers/pvec))

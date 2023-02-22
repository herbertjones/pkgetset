(uiop:define-package #:pkgetset.sycamore.test
  (:nicknames #:pkgetset.sycamore.test/all)
  (:use #:cl #:parachute)
  (:import-from #:pkgetset.sycamore.test/containers))
(cl:in-package #:pkgetset.sycamore.test)

(define-test top-level
  (test :pkgetset.sycamore.test/containers))

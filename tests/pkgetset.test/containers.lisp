(uiop:define-package #:pkgetset.test/containers
  (:use)
  (:mix #:cl #:parachute)
  (:import-from #:pkgetset.test/containers/array)
  (:import-from #:pkgetset.test/containers/cons)
  (:import-from #:pkgetset.test/containers/hash-table)
  (:import-from #:pkgetset.test/containers/vector))
(cl:in-package #:pkgetset.test/containers)

(define-test "getk ease of use"
  (test :pkgetset.test/containers/array)
  (test :pkgetset.test/containers/cons)
  (test :pkgetset.test/containers/hash-table)
  (test :pkgetset.test/containers/vector))

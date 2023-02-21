(uiop:define-package #:pkgetset.test/containers
  (:use)
  (:mix #:cl #:parachute)
  (:import-from #:pkgetset.test/containers/array)
  (:import-from #:pkgetset.test/containers/cons)
  (:import-from #:pkgetset.test/containers/hash-table)
  (:import-from #:pkgetset.test/containers/pk.gen)
  (:import-from #:pkgetset.test/containers/pk.str)
  (:import-from #:pkgetset.test/containers/pk.sym)
  (:import-from #:pkgetset.test/containers/pvec)
  (:import-from #:pkgetset.test/containers/vector))
(cl:in-package #:pkgetset.test/containers)

(define-test "getk ease of use"
  (test :pkgetset.test/containers/array)
  (test :pkgetset.test/containers/cons)
  (test :pkgetset.test/containers/hash-table)
  (test :pkgetset.test/containers/vector))

(define-test "persistent keyed"
  (test :pkgetset.test/containers/pk.gen)
  (test :pkgetset.test/containers/pk.str)
  (test :pkgetset.test/containers/pk.sym))

(define-test "persistent vector"
  (test :pkgetset.test/containers/pvec))

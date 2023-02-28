(uiop:define-package #:pkgetset.test
  (:nicknames #:pkgetset.test/all)
  (:use)
  (:mix #:cl #:parachute)
  (:import-from #:pkgetset.test/containers)
  (:import-from #:pkgetset.test/diff)
  (:import-from #:pkgetset.test/get-by-key)
  (:import-from #:pkgetset.test/set-by-key)
  (:import-from #:pkgetset.test/build))
(cl:in-package #:pkgetset.test)

(define-test top-level
  (test :pkgetset.test/containers)
  (test :pkgetset.test/diff)
  (test :pkgetset.test/get-by-key)
  (test :pkgetset.test/set-by-key)
  (test :pkgetset.test/build))

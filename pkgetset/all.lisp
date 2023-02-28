(uiop:define-package #:pkgetset
  (:nicknames #:pkgetset/all)
  (:use #:cl)
  (:use-reexport
   #:pkgetset/interfaces
   #:pkgetset/get-by-key
   #:pkgetset/containers
   #:pkgetset/set-by-key
   #:pkgetset/build-keyed
   #:pkgetset/diff))
(in-package #:pkgetset/all)

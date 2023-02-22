(uiop:define-package #:pkgetset/containers
  (:use #:cl)
  (:use-reexport #:pkgetset/containers/array
                 #:pkgetset/containers/cons
                 #:pkgetset/containers/hash-table
                 #:pkgetset/containers/pdict
                 #:pkgetset/containers/vector)
  (:documentation "Containers that implement interfaces from this library."))
(cl:in-package #:pkgetset/containers)

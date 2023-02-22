(uiop:define-package #:pkgetset.sycamore
  (:nicknames #:pkgetset.sycamore/all)
  (:use #:cl)
  (:use-reexport #:pkgetset.sycamore/pk.gen
                 #:pkgetset.sycamore/pk.str
                 #:pkgetset.sycamore/pk.sym
                 #:pkgetset.sycamore/pvec)
  (:documentation "Containers that wrap sycamore interfaces from this library."))
(cl:in-package #:pkgetset.sycamore)

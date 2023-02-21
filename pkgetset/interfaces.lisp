(uiop:define-package #:pkgetset/interfaces
  (:use)
  (:mix #:cl)
  (:use-reexport #:pkgetset/interfaces/keyed-container
                 #:pkgetset/interfaces/get-by-key
                 #:pkgetset/interfaces/keyed-transformers
                 #:pkgetset/interfaces/set-by-key
                 #:pkgetset/interfaces/diff)
  (:documentation "Persistent structure interfaces"))
(cl:in-package #:pkgetset/interfaces)

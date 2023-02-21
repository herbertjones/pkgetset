(uiop:define-package #:pkgetset/interfaces/diff
  (:use)
  (:mix #:pkgetset/interfaces/keyed-container
        #:pkgetset/interfaces/get-by-key
        #:pkgetset/interfaces/set-by-key
        #:cl)
  (:export
   #:diff-getk
   #:diff-setk
   #:diff-remk
   #:diff-fold-keyed)
  (:documentation "Get and set used by diff."))
(cl:in-package #:pkgetset/interfaces/diff)

(defgeneric diff-getk (keyed key &optional default)
  (:documentation "Gets a value by key from keyed data or returns the default.")
  (:method (keyed key &optional default)
    (getk keyed key default)))

(defgeneric diff-setk (settable key value)
  (:documentation "Set a key and return a new persistent container")
  (:method (settable key value)
    (setk settable key value)))

(defgeneric diff-remk (settable key)
  (:documentation "Remove a key and return a new persistent container")
  (:method (settable key)
    (remk settable key)))

(defgeneric diff-fold-keyed (obj f &optional initial-value)
  (:documentation "Walk over keys of an object.

F takes three arguments:
- accumulator
- key
- value")
  (:method (obj f &optional initial-value)
    (fold-keyed obj f initial-value)))

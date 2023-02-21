(uiop:define-package #:pkgetset/interfaces/set-by-key
  (:use)
  (:mix #:cl
        #:pkgetset/get-by-key)
  (:export #:settable-keyed-p
           #:settable-keyed
           #:setk
           #:remk
           #:settable-keyed->empty)
  (:documentation "Containers that can set a key and return a new container."))
(cl:in-package #:pkgetset/interfaces/set-by-key)

(defgeneric settable-keyed-p (settable)
  (:documentation "Test if keyed is also settable")
  (:method (obj)
    nil))

(deftype settable-keyed ()
  "Any type that implements the get-by-key generics."
  '(satisfies settable-keyed-p))

(defgeneric setk (settable key value)
  (:documentation "Set a key and return a new persistent container"))

(defgeneric remk (settable key)
  (:documentation "Remove a key and return a new persistent container"))

(defgeneric settable-keyed->empty (keyed)
  (:documentation "Get an empty container from a keyed object.")
  (:method (obj)
    ;; Default handler
    (error 'not-keyed :object obj)))

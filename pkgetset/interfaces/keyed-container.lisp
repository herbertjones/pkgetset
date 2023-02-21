(uiop:define-package #:pkgetset/interfaces/keyed-container
  (:use)
  (:mix #:cl)
  (:export
   #:not-keyed
   #:not-keyed-object
   #:keyedp
   #:keyed)
  (:documentation "Provide a way to identify keyed containers"))
(cl:in-package #:pkgetset/interfaces/keyed-container)

(define-condition not-keyed (error)
  ((object :reader not-keyed-object
           :initarg :object
           :documentation "The object that is not a keyed type."))
  (:documentation "Signaled when a non keyed object used where one is expected."))

(defgeneric keyedp (obj)
  (:documentation "Check if type supports keyed operations.")
  (:method (obj)
    nil))

(deftype keyed ()
  "Any type that implements the keyed generics."
  '(satisfies keyedp))

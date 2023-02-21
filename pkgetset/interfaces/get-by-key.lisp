(uiop:define-package #:pkgetset/interfaces/get-by-key
  (:use)
  (:mix #:pkgetset/interfaces/keyed-container
        #:cl)
  (:export #:getk
           #:fold-keyed
           #:lengthk)
  (:documentation "Access a keyed container by key"))
(cl:in-package #:pkgetset/interfaces/get-by-key)

(defgeneric getk (keyed key &optional default)
  (:documentation "Gets a value by key from keyed data or returns the default.")
  (:method (obj key &optional default)
    (declare (ignore default))
    (error 'not-keyed :object obj)))

(defgeneric fold-keyed (obj f &optional initial-value)
  (:documentation "Walk over keys of an object.

F takes three arguments:
- accumulator
- key
- value")
  (:method (obj f &optional initial-value)
    (declare (ignore initial-value))
    (error 'not-keyed :object obj)))

(defgeneric lengthk (keyed)
  (:documentation "Get the number of keys")
  (:method (obj)
    (fold-keyed obj (lambda (acc key value)
                      (declare (ignore key value))
                      (1+ acc))
                0)))

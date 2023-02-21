(uiop:define-package #:pkgetset/interfaces/keyed-transformers
  (:use)
  (:mix #:pkgetset/interfaces/keyed-container
        #:pkgetset/interfaces/get-by-key
        #:cl)
  (:export #:keyed-keys->list
           #:keyed-values->list
           #:keyed-keys->vector
           #:keyed-values->vector)
  (:documentation "Convert keyed into another container."))
(cl:in-package #:pkgetset/interfaces/keyed-transformers)

(defgeneric keyed-keys->list (obj)
  (:documentation
   "Get the keys to walk an object.")
  (:method (obj)
    (nreverse (fold-keyed obj
                          (lambda (acc key value)
                            (declare (ignore value))
                            (cons key acc))
                          '()))))

(defgeneric keyed-values->list (obj)
  (:documentation
   "Get the values of an object.")
  (:method (obj)
    (nreverse (fold-keyed obj
                          (lambda (acc key value)
                            (declare (ignore key))
                            (cons value acc))
                          '()))))

(defgeneric keyed-keys->vector (obj)
  (:documentation
   "Get the keys of an object as a vector.")
  (:method (obj)
    (let* ((size (lengthk obj))
           (pos 0)
           (vec (make-array size :initial-element nil)))
      (fold-keyed obj
                  (lambda (acc key value)
                    (declare (ignore acc value))
                    (setf (aref vec pos) key)
                    (incf pos)))
      vec)))

(defgeneric keyed-values->vector (obj)
  (:documentation
   "Get the values of an object as a vector.")
  (:method (obj)
    (let* ((size (lengthk obj))
           (pos 0)
           (vec (make-array size :initial-element nil)))
      (fold-keyed obj
                  (lambda (acc key value)
                    (declare (ignore acc key))
                    (setf (aref vec pos) value)
                    (incf pos)))
      vec)))

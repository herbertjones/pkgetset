(uiop:define-package #:pkgetset/containers/hash-table
  (:use)
  (:mix #:cl
        #:pkgetset/interfaces
        #:pkgetset/get-by-key)
  (:export)
  (:documentation "Implementation of getk methods for hash tables"))
(cl:in-package #:pkgetset/containers/hash-table)

(defmethod getk ((keyed hash-table) key &optional default)
  (gethash key keyed default))

(defmethod fold-keyed ((keyed hash-table) f &optional initial-value)
  (let ((acc initial-value))
    (maphash (lambda (key value)
               (setf acc (funcall f acc key value)))
             keyed)
    acc))

(defmethod keyedp ((keyed hash-table))
  (declare (ignore keyed))
  t)

(defmethod lengthk ((keyed hash-table))
  (hash-table-count keyed))

(defmethod keyed-keys->list ((keyed hash-table))
  (let ((keys (list)))
      (maphash #'(lambda (key value)
                   (declare (ignore value))
                   (push key keys))
               keyed)
      (nreverse keys)))

(defmethod keyed-values->list ((keyed hash-table))
  (let ((values (list)))
      (maphash #'(lambda (key value)
                   (declare (ignore key))
                   (push value values))
               keyed)
      (nreverse values)))

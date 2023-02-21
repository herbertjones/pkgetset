(uiop:define-package #:pkgetset/containers/vector
  (:use)
  (:mix #:cl
        #:pkgetset/interfaces
        #:pkgetset/get-by-key)
  (:export)
  (:documentation "Implementation of getk methods for vectors"))
(cl:in-package #:pkgetset/containers/vector)

(defmethod getk ((keyed vector) key &optional default)
  (if (array-in-bounds-p keyed key)
      (aref keyed key)
      default))

(defmethod fold-keyed ((keyed vector) f &optional initial-value)
  (loop :for var :across keyed
        :for pos :from 0
        :for acc := (funcall f initial-value pos var)
          :then (funcall f acc pos var)
        :finally (return acc)))

(defmethod keyedp ((keyed vector))
  (declare (ignore keyed))
  t)

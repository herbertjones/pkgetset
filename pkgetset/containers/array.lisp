(uiop:define-package #:pkgetset/containers/array
  (:use)
  (:mix #:cl
        #:pkgetset/interfaces
        #:pkgetset/get-by-key)
  (:export)
  (:documentation "Implementation of getk methods for arrays"))
(cl:in-package #:pkgetset/containers/array)

(defmethod getk ((keyed array) key &optional default)
  (typecase key
    (list
     (if (apply #'array-in-bounds-p keyed key)
         (apply #'aref keyed key)
         default))
    (fixnum
     (if (array-in-bounds-p keyed key)
         (aref keyed key)
         default))))

(defmethod fold-keyed ((keyed array) f &optional initial-value)
  (let* ((dims (array-dimensions keyed))
         (pos (loop :repeat (length dims) :collect 0)))
    (flet ((inc-pos ()
             (let ((state :inc-next))
               (setf pos
                     (loop :for dim-pos :from 0
                           :for max-val :in dims
                           :for pos-val :in pos
                           :collect (ecase state
                                      (:inc-next
                                       (cond ((< (1+ pos-val) max-val)
                                              (setf state :inc-done)
                                              (1+ pos-val))
                                             (t
                                              0)))
                                      (:inc-done
                                       pos-val))))
               (ecase state
                 (:inc-next nil)
                 (:inc-done t)))))
      (loop :with acc := initial-value
            :do (setf acc (funcall f acc pos (apply #'aref keyed pos)))
            :while (inc-pos)
            :finally (return acc)))))

(defmethod keyedp ((keyed array))
  (declare (ignore keyed))
  t)

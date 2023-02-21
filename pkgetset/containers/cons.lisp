(uiop:define-package #:pkgetset/containers/cons
  (:use)
  (:mix #:cl
        #:pkgetset/interfaces
        #:pkgetset/get-by-key)
  (:import-from #:pkgetset/util
                #:over-pairs)
  (:export)
  (:documentation "Implementation of getk methods for plists and alists."))
(cl:in-package #:pkgetset/containers/cons)

(defmethod getk ((keyed cons) key &optional default)
  (cond ((consp (car keyed))  ;; association list
         (dolist (pair keyed)
           (when (equal key (car pair))
             (return-from getk (cdr pair))))
         default)
        (t ;; plist
         (over-pairs keyed (maybe-key val)
           (when (equal key maybe-key)
             (return-from getk val)))
         default)))

(defmethod fold-keyed ((keyed cons) f &optional initial-value)
  (let ((acc initial-value))
    (cond ((consp (car keyed)) ;; association list
           (dolist (pair keyed)
             (setf acc (funcall f acc (car pair) (cdr pair)))))
          (t ;; plist
           (over-pairs keyed (key val)
             (setf acc (funcall f acc key val)))))
    acc))

(defmethod keyedp ((keyed cons))
  (declare (ignore keyed))
  t)

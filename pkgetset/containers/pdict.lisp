(uiop:define-package #:pkgetset/containers/pdict
  (:use)
  (:mix #:pkgetset/interfaces
        #:pkgetset/get-by-key
        #:cl)
  (:import-from #:alexandria-2
                #:copy-hash-table)
  (:export
   #:pdict)
  (:documentation "Persistent hash table"))
(cl:in-package #:pkgetset/containers/pdict)

(defstruct pdict
  "A naive persistent data structure that wraps hash-table."
  (inner (make-hash-table :test #'equal) :type hash-table :read-only t))

(defun pdict (&rest pairs)
  "Create a pdict from key value pairs."
  (make-pdict
   :inner (loop :with ht := (make-hash-table :test #'equal)
                :for data := pairs :then (cddr data)
                :while data
                :for key := (car data)
                :for value := (cadr data)
                :do (setf (gethash key ht) value)
                :finally (return ht))))

(declaim (type hash-table *empty-inner*))
(defparameter *empty-inner*
  (make-hash-table :test #'equal))

(declaim (type pdict *empty-object*))
(defparameter *empty-object*
  (make-pdict :inner *empty-inner*))

(defun hash-table->pdict (ht)
  (make-pdict :inner ht))

(defmethod getk ((keyed pdict) key &optional default)
  (gethash key (pdict-inner keyed) default))

(defmethod fold-keyed ((keyed pdict) f &optional initial-value)
  (let ((acc initial-value))
    (maphash (lambda (key value)
               (setf acc (funcall f acc key value)))
             (pdict-inner keyed))
    acc))

(defmethod keyedp ((keyed pdict))
  (declare (ignore keyed))
  t)

(defmethod lengthk ((keyed pdict))
  (hash-table-count (pdict-inner keyed)))

(defmethod keyed-keys->list ((keyed pdict))
  (let ((keys (list)))
      (maphash #'(lambda (key value)
                   (declare (ignore value))
                   (push key keys))
               (pdict-inner keyed))
      (nreverse keys)))

(defmethod keyed-values->list ((keyed pdict))
  (let ((values (list)))
      (maphash #'(lambda (key value)
                   (declare (ignore key))
                   (push value values))
               (pdict-inner keyed))
      (nreverse values)))

(defmethod settable-keyed-p ((settable pdict))
  t)

(defmethod settable-keyed->empty ((keyed pdict))
  *empty-object*)

(defmethod setk ((settable pdict) key value)
  (let ((copy (copy-hash-table (pdict-inner settable))))
    (setf (gethash key copy) value)
    (make-pdict :inner copy)))

(defmethod remk ((settable pdict) key)
  (if (getk? settable key)
      (let ((copy (copy-hash-table (pdict-inner settable))))
        (remhash key copy)
        (make-pdict :inner copy))
      settable))

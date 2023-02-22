(uiop:define-package #:pkgetset/containers/mutable-dict
  (:use)
  (:mix #:pkgetset/interfaces
        #:pkgetset/get-by-key
        #:pkgetset/containers/pdict
        #:cl)
  (:import-from #:alexandria-2
                #:copy-hash-table)
  (:export
   #:mutable-dict
   #:mutable-dict-finalize)
  (:documentation "Not so persistent hash table.

To be used as a mutable hash table that works with setk, but can be finalized
into a plist once mutation no longer needed."))
(cl:in-package #:pkgetset/containers/mutable-dict)

(defstruct mutable-dict
  "A mutable hash-table that can be finalized into a pdict."
  (inner (make-hash-table :test #'equal) :type (or null hash-table) :read-only nil))

(defun mutable-dict (&rest pairs)
  "Create a mutable-dict from key value pairs."
  (make-mutable-dict
   :inner (loop :with ht := (make-hash-table :test #'equal)
                :for data := pairs :then (cddr data)
                :while data
                :for key := (car data)
                :for value := (cadr data)
                :do (setf (gethash key ht) value)
                :finally (return ht))))

(defun mutable-dict-finalize (md)
  "Convert a mutable-dict into a pdict."
  (prog1 (hash-table->pdict (mutable-dict-inner md))
    (setf (mutable-dict-inner md) nil)))

(defun hash-table->mutable-dict (ht)
  (make-mutable-dict :inner ht))

(defmethod getk ((keyed mutable-dict) key &optional default)
  (gethash key (mutable-dict-inner keyed) default))

(defmethod fold-keyed ((keyed mutable-dict) f &optional initial-value)
  (let ((acc initial-value))
    (maphash (lambda (key value)
               (setf acc (funcall f acc key value)))
             (mutable-dict-inner keyed))
    acc))

(defmethod keyedp ((keyed mutable-dict))
  (declare (ignore keyed))
  t)

(defmethod lengthk ((keyed mutable-dict))
  (hash-table-count (mutable-dict-inner keyed)))

(defmethod keyed-keys->list ((keyed mutable-dict))
  (let ((keys (list)))
      (maphash #'(lambda (key value)
                   (declare (ignore value))
                   (push key keys))
               (mutable-dict-inner keyed))
      (nreverse keys)))

(defmethod keyed-values->list ((keyed mutable-dict))
  (let ((values (list)))
      (maphash #'(lambda (key value)
                   (declare (ignore key))
                   (push value values))
               (mutable-dict-inner keyed))
      (nreverse values)))

(defmethod settable-keyed-p ((settable mutable-dict))
  t)

(defmethod settable-keyed->empty ((keyed mutable-dict))
  (make-mutable-dict :inner (make-hash-table :test #'equal)))

(defmethod setk ((settable mutable-dict) key value)
  (setf (gethash key (mutable-dict-inner settable)) value)
  settable)

(defmethod remk ((settable mutable-dict) key)
  (remhash key (mutable-dict-inner settable))
  settable)

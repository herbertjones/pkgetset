(uiop:define-package #:pkgetset.sycamore/pvec/get
  (:use)
  (:mix #:pkgetset/get-by-key
        #:pkgetset/interfaces
        #:pkgetset.sycamore/pvec/pvec
        #:pkgetset.sycamore/pvec/raw-data
        #:cl)
  (:import-from #:alexandria-2
                #:iota)
  (:import-from #:sycamore
                #:tree-set
                #:tree-set-count
                #:fold-tree-set)
  (:export)
  (:documentation "Implementation of pkgetset/get-by-key generics for pvec"))
(cl:in-package #:pkgetset.sycamore/pvec/get)

(defmethod keyedp ((keyed pvec))
  (declare (ignore keyed))
  t)

(defmethod keyed-keys->list ((pv pvec))
  (let* ((ts (pvec-inner pv))
         (count (tree-set-count ts)))
    (iota count)))

(defmethod keyed-values->list ((pv pvec))
  (nreverse (fold-tree-set
             (lambda (acc raw-data)
               (cons (raw-data-value raw-data) acc))
             '()
             (pvec-inner pv))))

(defmethod lengthk ((pv pvec))
  (tree-set-count (pvec-inner pv)))

(defmethod fold-keyed ((keyed pvec) f &optional initial-value)
  (let ((pos 0))
    (fold-tree-set (lambda (acc raw-data)
                     (prog1 (funcall f acc pos (raw-data-value raw-data))
                       (incf pos)))
                   initial-value
                   (pvec-inner keyed))))

(defmethod getk ((pv pvec) key &optional default)
  (let ((raw-data (get-raw-by-key pv key)))
    (if raw-data
        (raw-data-value raw-data)
        default)))

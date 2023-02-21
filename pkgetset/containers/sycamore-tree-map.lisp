(uiop:define-package #:pkgetset/containers/sycamore-tree-map
  (:use)
  (:mix #:pkgetset/interfaces
        #:pkgetset/get-by-key
        #:cl)
  (:import-from #:sycamore
                #:empty-tree-map
                #:tree-map
                #:tree-map-count
                #:map-tree-map
                #:make-tree-map
                #:tree-map-insert
                #:tree-map-remove
                #:tree-map-keys
                #:tree-map-values
                #:fold-tree-map
                #:tree-map-find)
  (:import-from #:serapeum
                #:->)
  (:export #:wrapped-tree-map
           #:make-wrapped-tree-map
           #:wrapped-tree-map-p
           #:copy-wrapped-tree-map
           #:wrapped-tree-map-inner
           #:map-wrapped-tree-map-inner
           #:wrapped-tree-map-update
           #:wrapped-key-tree-map-add-from-alist
           #:wrapped-key-tree-map-add-from-pairs)
  (:documentation "Incomplete container for extension by containers that wrap a sycamore tree-map"))
(cl:in-package #:pkgetset/containers/sycamore-tree-map)

(defstruct wrapped-tree-map
  "A persistent data structure that wraps sycamore-tree-map."
  (inner nil :type tree-map :read-only t))

(defgeneric wrapped-tree-map-update (obj new-inner)
  (:documentation "Create new empty wrapped-tree-map subtype matching obj."))

(-> map-wrapped-tree-map-inner (wrapped-tree-map (-> (tree-map) tree-map)) t)
(defun map-wrapped-tree-map-inner (tm f)
  "Pass contents of TM through F and return new TM."
  (wrapped-tree-map-update tm
                           (funcall f (wrapped-tree-map-inner tm))))

(-> wrapped-key-tree-map-add-from-pairs (wrapped-tree-map list) t)
(defun wrapped-key-tree-map-add-from-pairs (wtm pairs)
  "Add to wrapped-tree-map using list of pairs."
  (map-wrapped-tree-map-inner
   wtm
   (lambda (tree-map)
     (loop for data = pairs then (cddr data)
           while data
           for key = (car data)
           for value = (cadr data)
           do (setf tree-map (tree-map-insert tree-map key value)))
     tree-map)))

(-> wrapped-key-tree-map-add-from-alist (wrapped-tree-map list) t)
(defun wrapped-key-tree-map-add-from-alist (wtm alist)
  "Add to wrapped-tree-map using alist."
  (map-wrapped-tree-map-inner
   wtm
   (lambda (tree-map)
     (reduce (lambda (tree-map pair)
               (tree-map-insert tree-map (car pair)
                                (if (consp (cdr pair))
                                    (wrapped-key-tree-map-add-from-alist
                                     (settable-keyed->empty wtm)
                                     (cdr pair))
                                    (cdr pair))))
             alist
             :initial-value tree-map))))

(defmethod settable-keyed->empty ((keyed wrapped-tree-map))
  (warn "Should override settable-keyed->empty")
  (wrapped-tree-map-update keyed
                           (empty-tree-map (wrapped-tree-map-inner keyed))))

(defmethod lengthk ((keyed wrapped-tree-map))
  (tree-map-count (wrapped-tree-map-inner keyed)))

(defmethod keyedp ((keyed wrapped-tree-map))
  (declare (ignore keyed))
  t)

(defmethod keyed-keys->list ((keyed wrapped-tree-map))
  (nreverse (tree-map-keys (wrapped-tree-map-inner keyed))))

(defmethod keyed-values->list ((keyed wrapped-tree-map))
  (nreverse (tree-map-values (wrapped-tree-map-inner keyed))))

(defmethod fold-keyed ((keyed wrapped-tree-map) f &optional initial-value)
  (fold-tree-map f initial-value (wrapped-tree-map-inner keyed)))

(defmethod getk ((keyed wrapped-tree-map) key &optional default)
  (tree-map-find (wrapped-tree-map-inner keyed) key default))

(defmethod settable-keyed-p ((settable wrapped-tree-map))
  t)

(defmethod setk ((settable wrapped-tree-map) key value)
  (map-wrapped-tree-map-inner settable
                              (lambda (tm)
                                (tree-map-insert tm key value))))

(defmethod remk ((settable wrapped-tree-map) key)
  (map-wrapped-tree-map-inner settable
                              (lambda (tm)
                                (tree-map-remove tm key))))

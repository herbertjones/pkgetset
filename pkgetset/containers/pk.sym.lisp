(uiop:define-package #:pkgetset/containers/pk.sym
  (:use #:cl
        #:pkgetset/interfaces
        #:pkgetset/get-by-key
        #:pkgetset/containers/sycamore-tree-map)
  (:import-from #:sycamore
                #:tree-map
                #:map-tree-map
                #:make-tree-map
                #:tree-map-count
                #:tree-map-insert
                #:tree-map-remove
                #:tree-map-keys
                #:tree-map-values
                #:fold-tree-map
                #:tree-map-find)
  (:import-from #:serapeum
                #:->)
  (:export #:pk.sym ;; class
           ;; class constructors
           #:empty-pk.sym
           #:alist->pk.sym
           #:pk.sym)
  (:documentation "pk.sym symbol keyed persistent structure with getk and setk implementation"))
(cl:in-package #:pkgetset/containers/pk.sym)

(-> compare-by-symbol (symbol symbol) (member -1 1 0))
(defun compare-by-symbol (a b)
  (let ((a-name (symbol-name a))
        (b-name (symbol-name b)))
    (cond ((string< a-name b-name) -1)
          ((string> a-name b-name) 1)
          (t (let ((a-package (symbol-package a))
                   (b-package (symbol-package b)))
               (cond ((not a) -1)
                     ((not b) 1)
                     (t (let ((a-package-name (package-name a-package))
                              (b-package-name (package-name b-package)))
                          (cond ((not a-package-name) -1)
                                ((not b-package-name) 1)
                                ((string< a-package-name b-package-name) -1)
                                ((string> a-package-name b-package-name) 1)
                                (t 0))))))))))

(defstruct (pk.sym (:include wrapped-tree-map))
  "A persistent data structure where the keys are symbols.")

(declaim (type tree-map *empty-inner*))
(defparameter *empty-inner*
  (make-tree-map #'compare-by-symbol))

(declaim (type pk.sym *empty-object*))
(defparameter *empty-object*
  (make-pk.sym :inner *empty-inner*))

(defmethod print-object ((obj pk.sym) stream)
  (print-unreadable-object (obj stream :type nil :identity nil)

    (pprint-logical-block (stream (map-tree-map :inorder 'list #'list (wrapped-tree-map-inner obj))
                                  :prefix "pk.sym{" :suffix "}")
      (do () (nil)
        (pprint-exit-if-list-exhausted)
        (let ((x (pprint-pop)))
          (pprint-newline :fill stream)
          (pprint-indent :block 0 stream)
          (pprint-logical-block (stream x)
            (write (car x) :stream stream)
            (write-char #\: stream)
            (write-char #\Space stream)
            (write (cadr x) :stream stream))
          (pprint-exit-if-list-exhausted)
          (write-char #\, stream)
          (write-char #\Space stream))))))

(defmethod settable-keyed->empty ((keyed pk.sym))
  (declare (ignore keyed))
  *empty-object*)

(defmethod wrapped-tree-map-update ((obj pk.sym) new-inner)
  (declare (ignore obj))
  (make-pk.sym :inner new-inner))

(-> alist->pk.sym (list) t)
(defun alist->pk.sym (alist)
  "Create a symbol keyed persistent keyed container from an alist."
  (wrapped-key-tree-map-add-from-alist *empty-object* alist))

(-> pk.sym (&rest t) t)
(defun pk.sym (&rest args)
  "Create a symbol keyed persistent keyed container from key value pairs."
  (wrapped-key-tree-map-add-from-pairs *empty-object* args))

(-> empty-pk.sym () pk.sym)
(defun empty-pk.sym ()
  "Empty pk.sym"
  *empty-object*)

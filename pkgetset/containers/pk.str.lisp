(uiop:define-package #:pkgetset/containers/pk.str
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
  (:export #:pk.str ;; class

           ;; class constructors
           #:empty-pk.str
           #:alist->pk.str
           #:pk.str)
  (:documentation "pk.str string keyed persistent structure with getk and setk implementation"))
(in-package #:pkgetset/containers/pk.str)

(-> compare-by-string (string string) (member -1 1 0))
(defun compare-by-string (a b)
  (cond ((string< a b) -1)
        ((string> a b) 1)
        (t 0)))

(defstruct (pk.str (:include wrapped-tree-map))
  "A persistent data structure where the keys are strings.")

(declaim (type tree-map *empty-inner*))
(defparameter *empty-inner*
  (make-tree-map #'compare-by-string))

(declaim (type pk.str *empty-object*))
(defparameter *empty-object*
  (make-pk.str :inner *empty-inner*))

(defmethod print-object ((obj pk.str) stream)
  (print-unreadable-object (obj stream :type nil :identity nil)

    (pprint-logical-block (stream (map-tree-map :inorder 'list #'list (wrapped-tree-map-inner obj))
                                  :prefix "pk.str{" :suffix "}")
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

(defmethod settable-keyed->empty ((keyed pk.str))
  (declare (ignore keyed))
  *empty-object*)

(defmethod wrapped-tree-map-update ((obj pk.str) new-inner)
  (declare (ignore obj))
  (make-pk.str :inner new-inner))

(-> alist->pk.str (list) t)
(defun alist->pk.str (alist)
  "Create a string keyed persistent keyed container from an alist."
  (wrapped-key-tree-map-add-from-alist *empty-object* alist))

(-> pk.str (&rest t) t)
(defun pk.str (&rest args)
  "Create a string keyed persistent keyed container from key value pairs."
  (wrapped-key-tree-map-add-from-pairs *empty-object* args))

(-> empty-pk.str () pk.str)
(defun empty-pk.str ()
  "Empty pk.str"
  *empty-object*)

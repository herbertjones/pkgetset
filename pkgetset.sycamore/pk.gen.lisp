(uiop:define-package #:pkgetset.sycamore/pk.gen
  (:use #:cl
        #:pkgetset/interfaces
        #:pkgetset/get-by-key
        #:pkgetset.sycamore/sycamore-tree-map)
  (:import-from #:sycamore
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
  (:export
   #:alist->pk.gen
   #:pk.gen
   #:empty-pk.gen)
  (:documentation "pk.gen generic keyed persistent structure with getk and setk implementation"))
(in-package #:pkgetset.sycamore/pk.gen)

(-> generic-compare-type-num (t) (member 0 1 2 3))
(defun generic-compare-type-num (value)
  (etypecase value
    (symbol 0)
    (string 1)
    (number 2)
    (cons 3)))

(-> compare-by-generic (t t) (member -1 1 0))
(defun compare-by-generic (a b)
  (cond ((and (symbolp a) (symbolp b))
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
        ((and (stringp a) (stringp b))
         (cond ((string< a b) -1)
               ((string> a b) 1)
               (t 0)))
        ((and (numberp a) (numberp b))
         (cond ((< a b) -1)
               ((> a b) 1)
               (t 0)))
        ((and (consp a) (consp b))
         (loop :for a-first := (car a) :then (car a-rest)
               :for b-first := (car b) :then (car b-rest)
               :for a-rest := (cdr a) :then (cdr a-rest)
               :for b-rest := (cdr b) :then (cdr b-rest)
               :do (progn
                     (case (compare-by-generic a-first b-first)
                       (-1 (return-from compare-by-generic -1))
                       (1 (return-from compare-by-generic 1)))
                     (cond ((and (consp a-rest) (consp b-rest))) ;; continue
                           ((consp a-rest) 1)                    ;; b wins
                           ((consp b-rest) -1)))) ;; a wins
         0)
        ((< (generic-compare-type-num a) (generic-compare-type-num b))
         -1)
        (t 1)))

(defstruct (pk.gen (:include wrapped-tree-map))
  "A generic persistent keyed container where the keys can be either
- integer
- string
- symbol")

(declaim (type tree-map *empty-inner*))
(defparameter *empty-inner*
  (make-tree-map #'compare-by-generic))

(declaim (type pk.gen *empty-object*))
(defparameter *empty-object*
  (make-pk.gen :inner *empty-inner*))

(defmethod print-object ((obj pk.gen) stream)
  (print-unreadable-object (obj stream :type nil :identity nil)

    (pprint-logical-block (stream (map-tree-map :inorder 'list #'list (wrapped-tree-map-inner obj))
                                  :prefix "pk.gen{" :suffix "}")
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

(defmethod settable-keyed->empty ((keyed pk.gen))
  (declare (ignore keyed))
  *empty-object*)

(defmethod wrapped-tree-map-update ((obj pk.gen) new-inner)
  (declare (ignore obj))
  (make-pk.gen :inner new-inner))

(-> alist->pk.gen (list) t)
(defun alist->pk.gen (alist)
  "Create a generic persistent keyed container from an alist."
  (wrapped-key-tree-map-add-from-alist *empty-object* alist))

(-> pk.gen (&rest t) t)
(defun pk.gen (&rest args)
  "Create a generic persistent keyed container from key value pairs."
  (wrapped-key-tree-map-add-from-pairs *empty-object* args))

(-> empty-pk.gen () pk.gen)
(defun empty-pk.gen ()
  "Empty pk.gen"
  *empty-object*)

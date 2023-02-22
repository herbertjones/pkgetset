(uiop:define-package #:pkgetset.sycamore/pvec/set
  (:use)
  (:mix #:pkgetset/interfaces
        #:pkgetset.sycamore/pvec/raw-data
        #:pkgetset.sycamore/pvec/pvec
        #:cl)
  (:import-from #:sycamore
                #:tree-set
                #:tree-set-count
                #:tree-set-replace
                #:tree-set-remove)
  (:documentation "Implementation of pkgetset/interface/set-by-key generics for pvec"))
(cl:in-package #:pkgetset.sycamore/pvec/set)

(defmethod settable-keyed-p ((pv pvec))
  t)

(defmethod settable-keyed->empty ((keyed pvec))
  (declare (ignore keyed))
  *empty-object*)

(defmethod setk ((pv pvec) key value)
  (let* ((ts (pvec-inner pv))
         (count (tree-set-count ts)))
    (cond ((zerop count)
           ;; Empty vector, add value as lone value
           (pvec value))
          ((eq key :start)
           (insert-at-start pv value))
          ((or (eq key :end)
               (and (numberp key)
                    (<= count key)))
           (insert-at-end pv value))
          ((and (consp key)
                (eq (first key) :insert))
           (pvec-insert-at-pos pv (second key) value))
          (t
           (let ((raw-data (get-raw-by-key pv key)))
             (unless raw-data
               (error "Missing ~S in ~S" key pv))
             (tree-set->pvec
              (tree-set-replace ts
                                (make-raw-data :id (raw-data-id raw-data)
                                               :value value))))))))

(defmethod remk ((pv pvec) key)
  (let* ((ts (pvec-inner pv))
         (count (tree-set-count ts)))
    (cond ((zerop count)
           ;; Empty.  Do nothing.
           pv)
          (t
           (let ((raw-data (get-raw-by-key pv key)))
             (cond ((not raw-data)
                    ;; Invalid key.  Do nothing.
                    pv)
                   (t
                    (tree-set->pvec
                     (tree-set-remove ts raw-data)))))))))

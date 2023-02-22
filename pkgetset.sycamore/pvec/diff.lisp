(uiop:define-package #:pkgetset.sycamore/pvec/diff
  (:use)
  (:mix #:pkgetset.sycamore/pvec/pvec
        #:pkgetset.sycamore/pvec/raw-data
        #:pkgetset/interfaces
        #:cl)
  (:import-from #:pkgetset/interfaces/diff)
  (:import-from #:serapeum
                #:mvlet*)
  (:import-from #:sycamore
                #:tree-set
                #:tree-set-find
                #:tree-set-count
                #:tree-set-remove
                #:tree-set-replace
                #:fold-tree-set)
  (:export)
  (:documentation "Diff interface implementation for pvec"))
(cl:in-package #:pkgetset.sycamore/pvec/diff)

(defmethod diff-getk ((keyed pvec) internal-key &optional default)
  (let ((found (get-raw-by-raw-id keyed internal-key 'not-found)))
    (if (eq found 'not-found)
        default
        (raw-data-value found))))

(defmethod diff-setk ((settable pvec) internal-key value)
  (tree-set->pvec
   (tree-set-replace (pvec-inner settable)
                     (make-raw-data :id internal-key
                                    :value value))))

(defmethod diff-remk ((settable pvec) internal-key)
  (tree-set->pvec (tree-set-remove (pvec-inner settable)
                                   (make-raw-data :id internal-key
                                                  :value nil))))

(defmethod diff-fold-keyed ((settable pvec) f &optional initial-value)
  (fold-tree-set (lambda (acc raw-data)
                   (funcall f acc
                            (raw-data-id raw-data)
                            (raw-data-value raw-data)))
                 initial-value
                 (pvec-inner settable)))

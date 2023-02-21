(uiop:define-package #:pkgetset/containers/pvec/compare
  (:use)
  (:import-from #:serapeum
                #:->)
  (:mix #:pkgetset/containers/pvec/raw-data
        #:cl)
  (:export
   #:compare-by-number-list-id)
  (:documentation "Compare operator for internal pvec usage"))
(cl:in-package #:pkgetset/containers/pvec/compare)

(-> compare-by-number-list (list list) (member -1 1 0))
(defun compare-by-number-list (a b)
  (let ((a-car (car a))
        (b-car (car b)))
    (cond ((and (not a-car) (not b-car)) 0)
          ((not a-car) -1)
          ((not b-car) 1)
          ((< a-car b-car) -1)
          ((> a-car b-car) 1)
          (t (compare-by-number-list (cdr a) (cdr b))))))

(-> compare-by-number-list-id (raw-data raw-data) (member -1 1 0))
(defun compare-by-number-list-id (ts1 ts2)
  "Comparison operator for our wrapped tree-set. Expects original data to be
wrapped, but sorting is done by the ID."
  (compare-by-number-list (raw-data-id ts1)
                          (raw-data-id ts2)))


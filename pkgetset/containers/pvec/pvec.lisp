(uiop:define-package #:pkgetset/containers/pvec/pvec
  (:use)
  (:mix #:pkgetset/interfaces
        #:pkgetset/get-by-key
        #:pkgetset/containers/pvec/raw-data
        #:pkgetset/containers/pvec/compare
        #:cl)
  (:import-from #:serapeum
                #:->
                #:mvlet*)
  (:import-from #:trivial-do
                #:doseq)
  (:import-from #:sycamore
                #:tree-set
                #:make-tree-set
                #:tree-set-count
                #:tree-set-find
                #:tree-set-position
                #:tree-set-ref
                #:tree-set-insert
                #:tree-set-min
                #:tree-set-max)
  (:export #:pvec
           #:make-pvec
           #:pvec-p
           #:copy-pvec
           #:pvec-inner
           #:tree-set->pvec
           #:pvec-get-position-of-id
           #:empty-pvec
           #:pvec-push
           #:list->pvec
           #:sequence->pvec
           #:get-raw-by-key
           #:insert-at-start
           #:insert-at-end
           #:*empty-tree*
           #:*empty-object*
           #:get-raw-by-raw-id
           #:get-raw-by-position
           #:pvec-insert-at-pos)
  (:documentation "struct for pvec"))
(cl:in-package #:pkgetset/containers/pvec/pvec)

(declaim (type tree-set *empty-tree*))
(defparameter *empty-tree*
  (make-tree-set #'compare-by-number-list-id))

(defstruct pvec
  "A persistent vector"
  (inner *empty-tree* :type tree-set))

(declaim (type pvec *empty-object*))
(defparameter *empty-object*
  (make-pvec :inner *empty-tree*))

(defmethod print-object ((obj pvec) stream)
  (print-unreadable-object (obj stream :type nil :identity nil)
    (pprint-logical-block (stream (nreverse (fold-keyed obj
                                                        (lambda (acc n value)
                                                          (cons (list n value) acc))))
                                  :prefix "pvec{" :suffix "}")
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

(-> pvec-get-position-of-id (pvec list) (values (or fixnum null) t &optional))
(defun pvec-get-position-of-id (pv id)
  "Get the position in the vec of the value identified by ID."
  (mvlet* ((ts (pvec-inner pv))
           (search-value (make-raw-data :id id :value nil))
           (full-value found (tree-set-find ts search-value)))
    (cond (found
           (values (tree-set-position ts search-value)
                   full-value))
          (t
           (values nil nil)))))

(-> get-raw-by-key (pvec (or keyword number)) (values (or null raw-data) &optional))
(defun get-raw-by-key (pv key)
  (let* ((ts (pvec-inner pv))
         (count (tree-set-count ts)))
    (cond ((zerop count)
           nil)
          ((keywordp key)
           (ecase key
             (:first
              (tree-set-min ts))
             (:last
              (tree-set-max ts))))
          ((numberp key)
           (if (or (<= count key)
                   (minusp key))
               nil ;; Past limits
               ;; Lookup value at position key
               (tree-set-ref ts key))))))

(-> get-raw-by-position (pvec number) (values (or null raw-data) &optional))
(defun get-raw-by-position (pv key)
  (let* ((ts (pvec-inner pv))
         (count (tree-set-count ts)))
    (cond ((zerop count)
           nil)
          ((numberp key)
           (if (or (<= count key)
                   (minusp key))
               nil ;; Past limits
               ;; Lookup value at position key
               (tree-set-ref ts key))))))

(defun get-raw-by-raw-id (pv raw-id &optional default-value)
  (mvlet* ((ts (pvec-inner pv))
           (search-value (make-raw-data :id raw-id :value nil))
           (full-value found (tree-set-find ts search-value)))
    (cond (found
           full-value)
          (t
           default-value))))

(-> empty-pvec () pvec)
(defun empty-pvec ()
  "Empty pvec"
  *empty-object*)

(-> tree-set->pvec (tree-set) pvec)
(defun tree-set->pvec (inner-tree-set)
  "Create a pvec from a pvec inner tree-set."
  (make-pvec :inner inner-tree-set))

(-> insert-at-start (pvec t) pvec)
(defun insert-at-start (pv value)
  (let* ((ts (pvec-inner pv))
         (last-raw-data (get-raw-by-key pv :first)))
    (if (not last-raw-data)
        (pvec value)
        (values (tree-set->pvec
                 (tree-set-insert ts (make-raw-data :id (raw-id-- (raw-data-id last-raw-data))
                                                    :value value)))))))

(-> insert-at-end (pvec t) pvec)
(defun insert-at-end (pv value)
  (let* ((ts (pvec-inner pv))
         (last-raw-data (get-raw-by-key pv :last)))
    (if (not last-raw-data)
        (pvec value)
        (tree-set->pvec
         (tree-set-insert ts (make-raw-data :id (raw-id++ (raw-data-id last-raw-data))
                                            :value value))))))

(-> pvec-push (pvec t) (values pvec &optional))
(defun pvec-push (pv value)
  "Append item to end of pvec."
  (insert-at-end pv value))

(defconstant +half-most-positive-fixnum+
  (coerce (floor most-positive-fixnum 2) 'fixnum))

(-> between-numeric-list (list list) list)
(defun between-numeric-list (ids-1 ids-2)
  "Two lists of numbers where the first is less than the second will provide a
 new one that goes between the two."
  (loop :with higher-carryover := 0
        :with build-return := '()
        :for lower := (car ids-1) :then (or (car lower-rest) 0)
        :for lower-rest := (cdr ids-1) :then (cdr lower-rest)
        :for higher := (car ids-2) :then (+ higher-carryover
                                            (or (car higher-rest) 0))
        :for higher-rest := (cdr ids-2) :then (cdr higher-rest)
        :do (progn
              (setf higher-carryover 0)
              (let ((diff (- higher lower)))
                (cond ((zerop diff)
                       ;; continue
                       (push lower build-return))
                      ((= 1 diff)
                       (push lower build-return)
                       ;; return lower
                       (setf higher-carryover +half-most-positive-fixnum+))
                      ((> diff 1)
                       (let ((between (+ lower (floor diff 2))))
                         (return-from between-numeric-list
                           (nreverse (cons between build-return)))))
                      (t
                       (error "higher is lower")))))))

(-> pvec-insert-at-pos (pvec fixnum t) pvec)
(defun pvec-insert-at-pos (pvec pos value)
  "Insert an item into a pvec at position"
  ;; Get position and position after
  (let ((raw-data-1 (get-raw-by-position pvec (1- pos)))
        (raw-data-2 (get-raw-by-position pvec pos)))
    (cond ((and raw-data-1 raw-data-2)
           (tree-set->pvec
            (tree-set-insert (pvec-inner pvec)
                             (make-raw-data
                              :id (between-numeric-list (raw-data-id raw-data-1)
                                                        (raw-data-id raw-data-2))
                              :value value))))
          (raw-data-1 (insert-at-end pvec value))
          (t (insert-at-start pvec value)))))

(-> list->pvec (list) pvec)
(defun list->pvec (list)
  "Create pvec from a list."
  (let ((ts *empty-tree*)
        (pos 0))
    (dolist (value list)
      (setf ts (tree-set-insert ts (make-raw-data :id (list pos) :value value)))
      (incf pos))
    (tree-set->pvec ts)))

(-> sequence->pvec (sequence) pvec)
(defun sequence->pvec (seq)
  "Create pvec from a sequence."
  (let ((ts *empty-tree*)
        (pos 0))
    (doseq (value seq)
      (setf ts (tree-set-insert ts (make-raw-data :id (list pos) :value value)))
      (incf pos))
    (tree-set->pvec ts)))

(-> pvec (&rest t) pvec)
(defun pvec (&rest list)
  "Create pvec from items"
  (list->pvec list))

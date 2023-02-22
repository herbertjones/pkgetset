(uiop:define-package #:pkgetset.sycamore.test/containers/pvec
  (:use)
  (:mix #:parachute
        #:pkgetset
        #:pkgetset.sycamore
        #:cl))
(cl:in-package #:pkgetset.sycamore.test/containers/pvec)

(defun test-pvec-1 ()
  (pvec :a :b :c))

(defun test-pvec-2 ()
  (pvec :one :two :three :four))

(defun test-pvec-3 ()
  (pvec 1 2 3 4))

(defun test-pvec-4 ()
  (pvec (test-pvec-1) (test-pvec-2)))

(define-test "Create pvec"
  (of-type pvec (pvec))
  (of-type pvec (pvec :a :b :c))
  (of-type pvec (test-pvec-1))
  (of-type pvec (empty-pvec)))

(define-test "getk"
  (let ((test-obj (test-pvec-1)))
    (is eql :a (getk test-obj 0))
    (is eql :b (getk test-obj 1))
    (is eql :c (getk test-obj 2))
    (is eql nil (getk test-obj 3))
    (fail (getk test-obj "s1"))))

(define-test "getk*"
  (let ((test-obj (test-pvec-1)))
    (is eql :a (getk* test-obj '(0)))
    (is eql :b (getk* test-obj '(1)))
    (is eql :c (getk* test-obj '(2)))
    (is eql nil (getk* test-obj '(3))))
  (let ((test-obj (test-pvec-4)))
    (is eql :a (getk* test-obj '(0 0)))
    (is eql :b (getk* test-obj '(0 1)))
    (is eql :c (getk* test-obj '(0 2)))
    (is eql nil (getk* test-obj '(0 3)))
    (is eql :one (getk* test-obj '(1 0)))))

(define-test "fold-keyed"
  ;; Test count
  (is = 3 (fold-keyed (test-pvec-1)
                      (lambda (acc key val)
                        (declare (ignore key val))
                        (1+ acc))
                      0))
  ;; Test values
  (is = 10 (fold-keyed (test-pvec-3)
                       (lambda (acc key val)
                         (declare (ignore key))
                         (+ acc val))
                       0)))

(define-test "lengthk"
  (is = 3 (lengthk (test-pvec-1)))
  (is = 4 (lengthk (test-pvec-2))))

(define-test "keyedp"
  (is eq t (keyedp (test-pvec-1)))
  (is eq t (keyedp (test-pvec-2))))

(define-test "keyed-keys->list"
  (is equalp '(0 1 2) (keyed-keys->list (test-pvec-1)))
  (is equalp '(0 1 2 3) (keyed-keys->list (test-pvec-2)))
  (let ((k (keyed-merge (test-pvec-1) (test-pvec-2))))
    (is equalp '(0 1 2 3) (keyed-keys->list k))
    (is equalp '(:a :b :c :four)
        (map 'list (lambda (key)
                     (getk k key))
             (keyed-keys->list k)))))

(define-test "keyed-values->list"
  (is equalp '(:a :b :c) (keyed-values->list (test-pvec-1))))

(define-test "keyed-keys->vector"
  (is equalp #(0 1 2) (keyed-keys->vector (test-pvec-1)))
  (is equalp #(0 1 2 3) (keyed-keys->vector (test-pvec-2)))
  (let ((k (keyed-merge (test-pvec-1) (test-pvec-2))))
    (is equalp #(0 1 2 3) (keyed-keys->vector k))
    (is equalp #(:a :b :c :four)
        (map 'vector (lambda (key)
                       (getk k key))
             (keyed-keys->vector k)))))

(define-test "keyed-values->vector"
  (is equalp #(:a :b :c) (keyed-values->vector (test-pvec-1))))

(define-test "setk"
  ;; Replacing a position
  (is eql :replacement (getk (setk (test-pvec-1) 0 :replacement) 0))
  (is eql :replacement (getk (setk (test-pvec-1) 2 :replacement) 2))
  ;; Append
  (is eql :new (getk (setk (test-pvec-1) 3 :new) 3))
  ;; Use special keywords
  (let ((pv (setk (test-pvec-1) :start :new)))
    (is eql :new (getk pv 0))
    (is eql :c (getk pv 3)))
  (let ((pv (setk (test-pvec-1) :end :new)))
    (is eql :new (getk pv 3))
    (is eql :a (getk pv 0)))
  (let ((pv (setk (test-pvec-1) '(:insert 0) :new)))
    (is eql :new (getk pv 0))
    (is eql :a (getk pv 1)))
  (let ((pv (setk (test-pvec-1) '(:insert 1) :new)))
    (is eql :new (getk pv 1))
    (is eql :a (getk pv 0))
    (is eql :b (getk pv 2))))

(define-test "remk"
  ;; Deleting a position shifts later positions lower
  (is eql :b (getk (remk (test-pvec-1) 0) 0))
  (is eql :c (getk (remk (test-pvec-1) 1) 1)))

(define-test "settable-keyed->empty"
  (of-type pvec (settable-keyed->empty (test-pvec-2)))
  (true (zerop (lengthk (settable-keyed->empty (test-pvec-2))))))

(define-test "diff"
  (dotimes (i 5)
    (let* ((original (pvec 1 2 3 4))
           (changed (pvec-insert-at-pos original i :new))
           (diff (diff-by-keys-find-deleted original changed)))
      (false (keyeds-equal-p original changed))
      (false (keyeds-equal-p changed diff))
      (false (keyeds-equal-p original diff))
      (true (keyeds-equal-p changed (apply-diff original diff))))))

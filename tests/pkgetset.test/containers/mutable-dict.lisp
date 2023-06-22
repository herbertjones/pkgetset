(uiop:define-package #:pkgetset.test/containers/mutable-dict
  (:use)
  (:mix #:parachute #:pkgetset #:cl))
(cl:in-package #:pkgetset.test/containers/mutable-dict)

(defun test-mutable-dict-1 ()
  (mutable-dict :a 1
                :b 2
                :c 3))

(defun test-mutable-dict-2 ()
  (mutable-dict :a 1
                :b 2
                :c (test-mutable-dict-1)))

(define-test "getk"
  (let ((test-obj (test-mutable-dict-1)))
    (is = (getk test-obj :a) 1)
    (is = (getk test-obj :c) 3)))

(define-test "fold-keyed"
  (is = 6 (fold-keyed (test-mutable-dict-1)
                      (lambda (acc key val)
                        (declare (ignore key val))
                        (+ acc val))
                      0)))

(define-test "lengthk"
  (is = 3 (lengthk (test-mutable-dict-1))))

(define-test "keyedp"
  (is eq t (keyedp (test-mutable-dict-1))))

(define-test "keyed-keys->list"
  (is equalp '(:a :b :c) (keyed-keys->list (test-mutable-dict-1))))

(define-test "keyed-values->list"
  (is equalp '(1 2 3) (keyed-values->list (test-mutable-dict-1))))

(define-test "keyed-keys->vector"
  (is equalp #(:a :b :c) (keyed-keys->vector (test-mutable-dict-1))))

(define-test "keyed-values->vector"
  (is equalp #(1 2 3) (keyed-values->vector (test-mutable-dict-1))))

(define-test "mutable-dict-finalize single"
  (let ((test-obj (mutable-dict-finalize-recursive (test-mutable-dict-2))))
    (is = (getk test-obj :a) 1)
    (is = (getk test-obj :b) 2)))

(define-test "pdict-getk"
  (let ((test-obj (mutable-dict-finalize-recursive (test-mutable-dict-2))))
    (is = (getk test-obj :a) 1)
    (is = (getk test-obj :b) 2)))

(define-test "mutable-dict-finalize makes pdict"
  (is eq
      'pdict
      (type-of (mutable-dict-finalize-recursive (test-mutable-dict-2)))))

(define-test "mutable-dict-finalize recursive makes pdict"
  (is eq
      'pdict
      (type-of (getk (mutable-dict-finalize-recursive (test-mutable-dict-2)) :c))))

(uiop:define-package #:pkgetset.test/containers/hash-table
  (:use)
  (:mix #:parachute #:pkgetset #:cl))
(cl:in-package #:pkgetset.test/containers/hash-table)

(defun test-hash-table-1 ()
  (serapeum:dict :a 1
                 :b 2
                 :c 3))

(define-test "getk"
  (let ((test-obj (test-hash-table-1)))
    (is = (getk test-obj :a) 1)
    (is = (getk test-obj :c) 3)))

(define-test "fold-keyed"
  (is = 6 (fold-keyed (test-hash-table-1)
                      (lambda (acc key val)
                        (declare (ignore key val))
                        (+ acc val))
                      0)))

(define-test "lengthk"
  (is = 3 (lengthk (test-hash-table-1))))

(define-test "keyedp"
  (is eq t (keyedp (test-hash-table-1))))

(define-test "keyed-keys->list"
  (is equalp '(:a :b :c) (keyed-keys->list (test-hash-table-1))))

(define-test "keyed-values->list"
  (is equalp '(1 2 3) (keyed-values->list (test-hash-table-1))))

(define-test "keyed-keys->vector"
  (is equalp #(:a :b :c) (keyed-keys->vector (test-hash-table-1))))

(define-test "keyed-values->vector"
  (is equalp #(1 2 3) (keyed-values->vector (test-hash-table-1))))

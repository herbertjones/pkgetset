(uiop:define-package #:pkgetset.test/containers/cons
  (:use)
  (:mix #:parachute #:pkgetset #:cl))
(cl:in-package #:pkgetset.test/containers/cons)

(defun test-plist-1 ()
  '(:a 1 :b 2 :c 3))

(defun test-alist-1 ()
  '((:a . 1)
    (:b . 2)
    (:c . 3)))

(define-test "getk"
  (let ((p1 (test-plist-1)))
    (is = (getk p1 :a) 1)
    (is = (getk p1 :c) 3))
  (let ((a1 (test-alist-1)))
    (is = (getk a1 :a) 1)
    (is = (getk a1 :c) 3)))

(define-test "fold-keyed"
  (is = 6 (fold-keyed (test-plist-1)
                      (lambda (acc key val)
                        (declare (ignore key val))
                        (+ acc val))
                      0))
  (is = 6 (fold-keyed (test-alist-1)
                      (lambda (acc key val)
                        (declare (ignore key val))
                        (+ acc val))
                      0)))

(define-test "lengthk"
  (is = 3 (lengthk (test-plist-1)))
  (is = 3 (lengthk (test-alist-1))))

(define-test "keyedp"
  (is eq t (keyedp (test-alist-1)))
  (is eq t (keyedp (test-plist-1))))

(define-test "keyed-keys->list"
  (is equalp '(:a :b :c) (keyed-keys->list (test-plist-1)))
  (is equalp '(:a :b :c) (keyed-keys->list (test-alist-1))))

(define-test "keyed-values->list"
  (is equalp '(1 2 3) (keyed-values->list (test-plist-1)))
  (is equalp '(1 2 3) (keyed-values->list (test-alist-1))))

(define-test "keyed-keys->vector"
  (is equalp #(:a :b :c) (keyed-keys->vector (test-plist-1)))
  (is equalp #(:a :b :c) (keyed-keys->vector (test-alist-1))))

(define-test "keyed-values->vector"
  (is equalp #(1 2 3) (keyed-values->vector (test-plist-1)))
  (is equalp #(1 2 3) (keyed-values->vector (test-alist-1))))

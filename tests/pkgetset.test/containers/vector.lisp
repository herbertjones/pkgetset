(uiop:define-package #:pkgetset.test/containers/vector
  (:use)
  (:mix #:parachute #:pkgetset #:cl))
(cl:in-package #:pkgetset.test/containers/vector)

(defun test-vector-1 ()
  #(1 2 3))

(define-test "getk"
  (let ((test-obj (test-vector-1)))
    (is = (getk test-obj 0) 1)
    (is = (getk test-obj 2) 3)))

(define-test "fold-keyed"
  (is = 6 (fold-keyed (test-vector-1)
                      (lambda (acc key val)
                        (declare (ignore key))
                        (+ acc val))
                      0)))

(define-test "lengthk"
  (is = 3 (lengthk (test-vector-1))))

(define-test "keyedp"
  (is eq t (keyedp (test-vector-1))))

(define-test "keyed-keys->list"
  (is equalp '(0 1 2) (keyed-keys->list (test-vector-1))))

(define-test "keyed-values->list"
  (is equalp '(1 2 3) (keyed-values->list (test-vector-1))))

(define-test "keyed-keys->vector"
  (is equalp #(0 1 2) (keyed-keys->vector (test-vector-1))))

(define-test "keyed-values->vector"
  (is equalp #(1 2 3) (keyed-values->vector (test-vector-1))))

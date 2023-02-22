(uiop:define-package #:pkgetset.test/containers/array
  (:use)
  (:mix #:parachute #:pkgetset #:cl))
(cl:in-package #:pkgetset.test/containers/array)

(defun test-array-1 ()
  #2A((1 2 3)
      (4 5 6)))

(define-test "getk"
  (let ((test-obj (test-array-1)))
    (is = (getk test-obj '(0 0)) 1)
    (is = (getk test-obj '(0 2)) 3)
    (is = (getk test-obj '(1 0)) 4)
    (is = (getk test-obj '(1 2)) 6)))

(define-test "fold-keyed"
  (is = 6 (fold-keyed (test-array-1)
                      (lambda (acc key val)
                        (declare (ignore key val))
                        (1+ acc))
                      0)))

(define-test "lengthk"
  (is = 6 (lengthk (test-array-1))))

(define-test "keyedp"
  (is eq t (keyedp (test-array-1))))

(define-test "keyed-keys->list"
  (is equalp
      '((0 0) (1 0) (0 1) (1 1) (0 2) (1 2))
      (keyed-keys->list (test-array-1))))

(define-test "keyed-values->list"
  (is equalp
      '(1 4 2 5 3 6)
      (keyed-values->list (test-array-1))))

(define-test "keyed-keys->vector"
  (is equalp
      #((0 0) (1 0) (0 1) (1 1) (0 2) (1 2))
      (keyed-keys->vector (test-array-1))))

(define-test "keyed-values->vector"
  (is equalp
      #(1 4 2 5 3 6)
      (keyed-values->vector (test-array-1))))

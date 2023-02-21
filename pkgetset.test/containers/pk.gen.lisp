(uiop:define-package #:pkgetset.test/containers/pk.gen
  (:use)
  (:mix #:parachute #:pkgetset #:cl))
(cl:in-package #:pkgetset.test/containers/pk.gen)

(defun test-pk.gen-1 ()
  (pk.gen :strings (pk.gen "a" :a
                           "b" :b)
          :numbers (pk.gen 1 :one
                           2 :two)
          :mixed (pk.gen 1 :one
                         2 :two
                         "a" :a
                         "b" :b
                         :c "c"
                         :d "d")
          :n0 0
          :s0 "0"
          0 'a-zero))

(defun test-pk.gen-2 ()
  (pk.gen :one 1
          :two 2
          :three 3
          :four 4))

(defun test-pk.gen-3 ()
  (pk.gen "one" 1
          "two" 2
          "three" 3
          "four" 4))

(defun test-pk.gen-4 ()
  (pk.gen 1 1
          2 2
          3 3
          4 4))

(defun test-pk.gen-5 ()
  (pk.gen "five" 5
          "six" 6
          "one" -1))

(define-test "Create pk.gen"
  (of-type pk.gen (pk.gen))
  (of-type pk.gen (pk.gen :a 1 :b 2 :c 3))
  (of-type pk.gen (pk.gen "a" 1 "b" 2 "c" 3))
  (of-type pk.gen (pk.gen 0 "a" 1 "b" 2 "c"))
  (of-type pk.gen (pk.gen :a 1 "b" 2 3 "c"))
  (of-type pk.gen (pk.gen :a 1 "b" 2 3 "c"))
  (of-type pk.gen (test-pk.gen-1))
  (of-type pk.gen (empty-pk.gen)))

(define-test "getk"
  (let ((test-obj (test-pk.gen-1)))
    (is eq 'a-zero (getk test-obj 0))
    (is = 0 (getk test-obj :n0))
    (is equal "0" (getk test-obj :s0))
    (is eq nil (getk test-obj :s1))))

(define-test "getk*"
  (let ((test-obj (test-pk.gen-1)))
    ;; Single
    (is eq 'a-zero (getk* test-obj '(0)))
    (is = 0 (getk* test-obj '(:n0)))
    (is equal "0" (getk* test-obj '(:s0)))

    ;; Nested
    (is eq :a (getk* test-obj '(:strings "a")))
    (is eq :b (getk* test-obj '(:strings "b")))
    (is eq :one (getk* test-obj '(:numbers 1)))
    (is eq :two (getk* test-obj '(:numbers 2)))
    (is eq nil (getk* test-obj '(:nothing :nothing :there)))))

(define-test "fold-keyed"
  ;; Test count
  (is = 6 (fold-keyed (test-pk.gen-1)
                      (lambda (acc key val)
                        (declare (ignore key val))
                        (1+ acc))
                      0))
  ;; Test values
  (is = 10 (fold-keyed (test-pk.gen-2)
                       (lambda (acc key val)
                         (declare (ignore key))
                         (+ acc val))
                       0))
  ;; Test keys
  (is = 3 (fold-keyed (test-pk.gen-2)
                      (lambda (acc key val)
                        (if (member key '(:one :two))
                            (+ acc val)
                            acc))
                      0)))

(define-test "lengthk"
  (is = 6 (lengthk (test-pk.gen-1)))
  (is = 4 (lengthk (test-pk.gen-2))))

(define-test "keyedp"
  (is eq t (keyedp (test-pk.gen-1)))
  (is eq t (keyedp (test-pk.gen-2))))

(define-test "keyed-keys->list"
  (is equalp
      '(:four :one :three :two)
      (keyed-keys->list (test-pk.gen-2)))
  (is equalp
      '("four" "one" "three" "two")
      (keyed-keys->list (test-pk.gen-3)))
  (let ((k (keyed-merge (test-pk.gen-5) (test-pk.gen-3))))
    (is equalp
        '("five" "four" "one" "six" "three" "two")
        (keyed-keys->list k))
    (is equalp
        '(5 4 -1 6 3 2)
        (map 'list (lambda (key) (getk k key)) (keyed-keys->list k))))
  (is equalp
      '(1 2 3 4)
      (keyed-keys->list (test-pk.gen-4)))
  (is equalp
      '(:four :one :three :two  "four" "one" "three" "two" 1 2 3 4)
      (keyed-keys->list (keyed-merge
                         (test-pk.gen-2)
                         (test-pk.gen-3)
                         (test-pk.gen-4)))))

(define-test "keyed-values->list"
  (is equalp
      '(4 1 3 2)
      (keyed-values->list (test-pk.gen-2))))

(define-test "keyed-keys->vector"
  (is equalp
      #(:four :one :three :two)
      (keyed-keys->vector (test-pk.gen-2))))

(define-test "keyed-values->vector"
  (is equalp
      #(4 1 3 2)
      (keyed-values->vector (test-pk.gen-2))))

(define-test "setk"
  (is eql -1 (getk (setk (test-pk.gen-2) :one -1) :one))
  (is eql -1 (getk (setk (test-pk.gen-2) :new -1) :new)))

(define-test "remk"
  (is eql nil (getk (remk (test-pk.gen-2) :one) :one))
  (is eql nil (getk (remk (test-pk.gen-2) :new) :new)))

(define-test "settable-keyed->empty"
  (of-type pk.gen (settable-keyed->empty (test-pk.gen-3)))
  (true (zerop (lengthk (settable-keyed->empty (test-pk.gen-3))))))

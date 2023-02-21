(uiop:define-package #:pkgetset.test/containers/pk.str
  (:use)
  (:mix #:parachute #:pkgetset #:cl))
(cl:in-package #:pkgetset.test/containers/pk.str)

(defun test-pk.str-1 ()
  (pk.str "strings" (pk.str "a" :a
                            "b" :b)
          "n0" 0
          "s0" "0"))

(defun test-pk.str-2 ()
  (pk.str "one" 1
          "two" 2
          "three" 3
          "four" 4))

(defun test-pk.str-3 ()
  (pk.str "1" 1
          "2" 2
          "3" 3
          "4" 4))

(defun test-pk.str-4 ()
  (pk.str "five" 5
          "six" 6
          "one" -1))

(define-test "Create pk.str"
  (of-type pk.str (pk.str))
  (of-type pk.str (pk.str "a" 1 "b" 2 "c" 3))
  (of-type pk.str (test-pk.str-1))
  (of-type pk.str (empty-pk.str)))

(define-test "getk"
  (let ((test-obj (test-pk.str-1)))
    (is eql 0 (getk test-obj "n0"))
    (is = 0 (getk test-obj "n0"))
    (is equal "0" (getk test-obj "s0"))
    (is eq nil (getk test-obj "s1"))))

(define-test "getk*"
  (let ((test-obj (test-pk.str-1)))
    ;; Single
    (is = 0 (getk* test-obj '("n0")))
    (is equal "0" (getk* test-obj '("s0")))

    ;; Nested
    (is eq :a (getk* test-obj '("strings" "a")))
    (is eq :b (getk* test-obj '("strings" "b")))
    (is eq nil (getk* test-obj '("nothing" "nothing" "there")))))

(define-test "fold-keyed"
  ;; Test count
  (is = 3 (fold-keyed (test-pk.str-1)
                      (lambda (acc key val)
                        (declare (ignore key val))
                        (1+ acc))
                      0))
  ;; Test values
  (is = 10 (fold-keyed (test-pk.str-2)
                       (lambda (acc key val)
                         (declare (ignore key))
                         (+ acc val))
                       0)))

(define-test "lengthk"
  (is = 3 (lengthk (test-pk.str-1)))
  (is = 4 (lengthk (test-pk.str-2))))

(define-test "keyedp"
  (is eq t (keyedp (test-pk.str-1)))
  (is eq t (keyedp (test-pk.str-2))))

(define-test "keyed-keys->list"
  (is equalp
      '("four" "one" "three" "two")
      (keyed-keys->list (test-pk.str-2)))
  (is equalp
      '("four" "one" "three" "two")
      (keyed-keys->list (test-pk.str-2)))
  (let ((k (keyed-merge (test-pk.str-4) (test-pk.str-2))))
    (is equalp
        '("five" "four" "one" "six" "three" "two")
        (keyed-keys->list k))
    (is equalp
        '(5 4 -1 6 3 2)
        (map 'list (lambda (key) (getk k key)) (keyed-keys->list k))))
  (is equalp
      '("1" "2" "3" "4")
      (keyed-keys->list (test-pk.str-3)))
  (is equalp
      '("1" "2" "3" "4" "four" "one" "three" "two")
      (keyed-keys->list (keyed-merge
                         (test-pk.str-2)
                         (test-pk.str-2)
                         (test-pk.str-3)))))

(define-test "keyed-values->list"
  (is equalp
      '(4 1 3 2)
      (keyed-values->list (test-pk.str-2))))

(define-test "keyed-keys->vector"
  (is equalp
      #("four" "one" "three" "two")
      (keyed-keys->vector (test-pk.str-2))))

(define-test "keyed-values->vector"
  (is equalp
      #(4 1 3 2)
      (keyed-values->vector (test-pk.str-2))))

(define-test "setk"
  (is eql -1 (getk (setk (test-pk.str-2) "one" -1) "one"))
  (is eql -1 (getk (setk (test-pk.str-2) "new" -1) "new")))

(define-test "remk"
  (is eql nil (getk (remk (test-pk.str-2) "one") "one"))
  (is eql nil (getk (remk (test-pk.str-2) "new") "new")))

(define-test "settable-keyed->empty"
  (of-type pk.str (settable-keyed->empty (test-pk.str-2)))
  (true (zerop (lengthk (settable-keyed->empty (test-pk.str-2))))))

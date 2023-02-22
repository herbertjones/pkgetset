(uiop:define-package #:pkgetset.test/get-by-key
  (:use)
  (:mix #:parachute #:pkgetset #:cl))
(cl:in-package #:pkgetset.test/get-by-key)

(defun test-obj-1 ()
  (pdict :a (pdict :aa :aa-value
                   :ab :ab-value)
         :b 1
         :n (pdict :n1 10
                   :n2 200)))

(define-test "getk?"
  (true (getk? (test-obj-1) :a))
  (false (getk? (test-obj-1) :c)))

(define-test "getk*?"
  (true (getk*? (test-obj-1) '(:a :aa)))
  (false (getk*? (test-obj-1) '(:a :ac)))
  (false (getk*? (test-obj-1) '(:c))))

(define-test "getk*"
  (is eq :aa-value (getk* (test-obj-1) '(:a :aa)))
  (is eq nil (getk* (test-obj-1) '(:a :ac)))
  (is eq nil (getk* (test-obj-1) '(:c))))

(define-test "egetk"
  (is eql 1 (egetk (test-obj-1) :b))
  (fail (egetk (test-obj-1) :c)))

(define-test "egetk*"
  (is eql :aa-value (egetk* (test-obj-1) '(:a :aa)))
  (fail (egetk* (test-obj-1) '(:a :ac)))
  (fail (egetk* (test-obj-1) '(:c))))

(define-test "with-keyed"
  (is = 1 (with-keyed (test-obj-1) (:b b)
            b))
  (is eq :ab-value (with-keyed (test-obj-1) (:a (:ab ab))
                     ab))
  (is = 211 (with-keyed (test-obj-1) (:n (:n1 n1
                                          :n2 n2)
                                      :b b)
              (+ n1 n2 b)))
  (is eq nil (with-keyed (test-obj-1) (:a (:ac ac))
               ac)))

(define-test "ewith-keyed"
  (is = 1 (ewith-keyed (test-obj-1) (:b b)
            b))
  (is eq :ab-value (ewith-keyed (test-obj-1) (:a (:ab ab))
                     ab))
  (is = 211 (ewith-keyed (test-obj-1) (:n (:n1 n1
                                           :n2 n2)
                                       :b b)
              (+ n1 n2 b)))
  (fail (ewith-keyed (test-obj-1) (:a (:ac ac))
          ac)))

(define-test "over-keyed"
  ;; Is not recursive.
  (is = 1 (over-keyed ((test-obj-1) :value val
                                    :accumulator acc
                                    :initial 0)
            (if (numberp val) (+ val acc) acc))))

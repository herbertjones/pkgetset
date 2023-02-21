(uiop:define-package #:pkgetset.test/set-by-key
  (:use)
  (:mix #:parachute #:pkgetset #:cl))
(cl:in-package #:pkgetset.test/set-by-key)

(defun test-obj-1 ()
  (pk.sym :a (pk.sym :aa :aa-value
                     :ab :ab-value)
          :b 1
          :n (pk.sym :n1 10
                     :n2 200)))

(define-test "setk!"
  (let ((obj (test-obj-1)))
    (setk! obj :b 2)
    (is = 2 (getk obj :b))))

(define-test "setk*"
  (is eq
      :new-value
      (getk* (setk* (test-obj-1)
                    '(:a :aa)
                    :new-value)
             '(:a :aa))))

(define-test "setk*!"
  (let ((obj (test-obj-1)))
    (setk*! obj '(:a :aa) :new-value)
    (is eq :new-value (getk* obj '(:a :aa)))))

(define-test "modk"
  (is = 2 (getk (modk (test-obj-1) :b #'1+)
                :b))
  (is = 1 (getk (modk (test-obj-1) :c #'1+ 0)
                :c)))

(define-test "modk*"
  (is = 11 (getk* (modk* (test-obj-1) '(:n :n1) #'1+)
                  '(:n :n1))))

(define-test "emodk"
  (is = 2 (getk (emodk (test-obj-1) :b #'1+)
                :b))
  (fail (emodk (test-obj-1) :c #'1+)))

(define-test "emodk*"
  (is = 11 (getk* (emodk* (test-obj-1) '(:n :n1) #'1+)
                 '(:n :n1)))
  (fail (emodk* (test-obj-1) '(:n :n3) #'1+)))

(define-test "modk!"
  (let ((obj (test-obj-1)))
    (modk! obj :b #'1+)
    (is = 2 (getk obj :b)))
  (let ((obj (test-obj-1)))
    (modk! obj :c #'1+ 0)
    (is = 1 (getk obj :c))))

(define-test "modk*!"
  (let ((obj (test-obj-1)))
    (modk*! obj '(:n :n1) #'1+)
    (is = 11 (getk* obj '(:n :n1))))
  (let ((obj (test-obj-1)))
    (modk*! obj '(:n :n3) #'1+ 0)
    (is = 1 (getk* obj '(:n :n3)))))

(define-test "emodk!"
  (let ((obj (test-obj-1)))
    (emodk! obj :b #'1+)
    (is = 2 (getk obj :b)))
  (let ((obj (test-obj-1)))
    (fail (emodk! obj :c #'1+))))

(define-test "emodk*!"
  (let ((obj (test-obj-1)))
    (emodk*! obj '(:n :n1) #'1+)
    (is = 11 (getk* obj '(:n :n1))))
  (let ((obj (test-obj-1)))
    (fail (emodk*! obj '(:n :n3) #'1+))))

(define-test "remk!"
  (let ((obj (test-obj-1)))
    (remk! obj :b)
    (is eq nil (getk obj :b))))

(define-test "remk*"
  (is eq nil
      (getk* (remk* (test-obj-1) '(:a :aa))
             '(:a :aa))))

(define-test "remk*!"
  (let ((obj (test-obj-1)))
    (remk*! obj '(:a :aa))
    (is eq nil (getk* obj '(:a :aa)))))

(define-test "keyed-merge"
  (true (keyeds-equal-p (keyed-merge (pk.sym :a 1 :b 2)
                                     (pk.sym :b 3 :c 4))
                        (pk.sym :a 1 :b 2 :c 4))))

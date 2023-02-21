(uiop:define-package #:pkgetset.test/diff
  (:use)
  (:mix #:parachute #:pkgetset #:cl))
(cl:in-package #:pkgetset.test/diff)

(define-test "diff recognizes new keys"
  (true (keyeds-equal-p (diff-by-keys (pk.sym :a 1
                                              :b 2)
                                      (pk.sym :a 1
                                              :b 2
                                              :c 3))
                        (pk.sym :c 3)))
  (true (keyeds-equal-p (diff-by-keys (pk.sym :b 2
                                              :c 3)
                                      (pk.sym :a 1
                                              :b 2
                                              :c 3))
                        (pk.sym :a 1)))
  (true (keyeds-equal-p (diff-by-keys (pk.sym :a (pk.sym :aa 1
                                                         :ab 2))
                                      (pk.sym :a (pk.sym :aa 1
                                                         :ab 2
                                                         :ac 3)))
                        (pk.sym :a (pk.sym :ac 3)))))

(define-test "diff recognizes removed keys"
  (true (keyeds-equal-p
         (diff-by-keys-find-deleted (pk.sym :a 1
                                            :b 2
                                            :c 3)
                                    (pk.sym :a 1
                                            :b 2))
         (pk.sym :c :deleted)))
  (true (keyeds-equal-p
         (diff-by-keys-find-deleted (pk.sym :a 1
                                            :b 2
                                            :c 3)
                                    (pk.sym :b 2
                                            :c 3))
         (pk.sym :a :deleted)))
  (true (keyeds-equal-p
         (diff-by-keys-find-deleted (pk.sym :a (pk.sym :aa 1
                                                       :ab 2
                                                       :ac 3))
                                    (pk.sym :a (pk.sym :aa 1
                                                       :ab 2)))
         (pk.sym :a (pk.sym :ac :deleted)))))

(define-test "apply-diff applies diff"
  (let* ((original (pk.sym :a 1
                           :b 2
                           :c 3))
         (changed (pk.sym :a 1
                          :c 3))
         (diff (diff-by-keys-find-deleted original changed)))
    (false (keyeds-equal-p original changed))
    (false (keyeds-equal-p changed diff))
    (false (keyeds-equal-p original diff))
    (true (keyeds-equal-p changed (apply-diff original diff))))

  (let* ((original (pk.sym :a 1
                           :c 3))
         (changed (pk.sym :a 1
                          :b 2
                          :c 3))
         (diff (diff-by-keys-find-deleted original changed)))
    (false (keyeds-equal-p original changed))
    (false (keyeds-equal-p changed diff))
    (false (keyeds-equal-p original diff))
    (true (keyeds-equal-p changed (apply-diff original diff))))

  (let* ((original (pk.sym :a (pk.sym :a 1
                                      :b 2
                                      :c 3)))
         (changed (pk.sym :a (pk.sym :a 1
                                     :c 3)))
         (diff (diff-by-keys-find-deleted original changed)))
    (false (keyeds-equal-p original changed))
    (false (keyeds-equal-p changed diff))
    (false (keyeds-equal-p original diff))
    (true (keyeds-equal-p changed (apply-diff original diff))))

  (let* ((original (pk.sym :a (pk.sym :a 1
                                      :c 3)))
         (changed (pk.sym :a (pk.sym :a 1
                                     :b 2
                                     :c 3)))
         (diff (diff-by-keys-find-deleted original changed)))
    (false (keyeds-equal-p original changed))
    (false (keyeds-equal-p changed diff))
    (false (keyeds-equal-p original diff))
    (true (keyeds-equal-p changed (apply-diff original diff)))))

(define-test "diffs-in-conflict-p"
  ;; Not in conflict as parts do not touch
  (let* ((original (pk.sym :a 1
                           :b 2
                           :c 3))
         (changed1 (pk.sym :a 1
                           :c 3))
         (changed2 (pk.sym :a 1
                           :b 2))
         (diff1 (diff-by-keys-find-deleted original changed1))
         (diff2 (diff-by-keys-find-deleted original changed2)))
    (false (keyeds-equal-p original changed1))
    (false (keyeds-equal-p changed1 diff1))
    (false (keyeds-equal-p original diff1))
    (false (keyeds-equal-p original changed2))
    (false (keyeds-equal-p changed2 diff2))
    (false (keyeds-equal-p original diff2))
    (false (diffs-in-conflict-p diff1 diff2)))

  ;; In conflict as parts do touch
  (let* ((original (pk.sym :a 1
                           :b 2
                           :c 3))
         (changed1 (pk.sym :a 1
                           :b 3
                           :c 3))
         (changed2 (pk.sym :a 1
                           :b 4
                           :c 3))
         (diff1 (diff-by-keys-find-deleted original changed1))
         (diff2 (diff-by-keys-find-deleted original changed2)))
    (false (keyeds-equal-p original changed1))
    (false (keyeds-equal-p changed1 diff1))
    (false (keyeds-equal-p original diff1))
    (false (keyeds-equal-p original changed2))
    (false (keyeds-equal-p changed2 diff2))
    (false (keyeds-equal-p original diff2))
    (true (diffs-in-conflict-p diff1 diff2))))

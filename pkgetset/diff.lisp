(uiop:define-package #:pkgetset/diff
  (:use #:cl
        #:pkgetset/containers/pdict
        #:pkgetset/containers/mutable-dict
        #:pkgetset/interfaces
        #:pkgetset/get-by-key)
  (:import-from #:serapeum
                #:->)
  (:export #:diff-by-keys
           #:diff-by-keys-find-deleted
           #:diffs-in-conflict-p
           #:apply-diff
           #:keyeds-equal-p))
(in-package #:pkgetset/diff)

(defun diff-getk? (keyed key)
  "(package internal)  Version of getk? for diff-getk."
  (not (eq 'no-key (diff-getk keyed key 'no-key))))

(defmacro diff-over-keyed ((keyed &rest arg-plist) &body body)
  "(package internal)  Iterate over keyed data.

arg-plist takes the following keyword arguments:
  :key KEY
  :value VALUE
  :accumulator PREVIOUS-RESULT
  :initial FIRST-ACCUMULATOR

Example:
    (diff-over-keyed (kdata :key k)
      (format t \"Key: ~A~%\" k))"
  (let ((key (or (getf arg-plist :key) (gensym "key")) )
        (value (or (getf arg-plist :value) (gensym "value")))
        (acc (or (getf arg-plist :accumulator) (gensym "accumulator"))))
    `(diff-fold-keyed ,keyed
                      (lambda (,acc ,key ,value)
                        (declare (ignorable ,acc ,key ,value))
                        ,@body)
                      ,(getf arg-plist :initial))))

(-> reduce-keys-union ((-> (mutable-dict t) mutable-dict)
                       mutable-dict
                       keyed
                       keyed)
  t)
(defun reduce-keys-union (f initial-value keyed-a keyed-b)
  "(package internal)  Iterate over the union of the keys of two keyed containers.

INITIAL-VALUE
    The initial value to pass to F.

F
    Takes a reducer function F that takes
    1. The initial value, then the previous return of F.
    2. Key from KEYED-A and/or KEYED-B.

KEYED-A
    Container with keys to pass to F

KEYED-B
    Container with keys to pass to F"
  (let ((result1 (diff-over-keyed (keyed-a :key key
                                           :accumulator acc
                                           :initial initial-value)
                   (funcall f acc key))))
    (diff-over-keyed (keyed-b :key key
                              :accumulator acc
                              :initial result1)
      (if (diff-getk? keyed-a key)
          acc
          (funcall f acc key)))))

(defun diff-by-keys (a b &key (test #'equal))
  "Diff two keyed objects, finding new and changed entries.

If looking to catch deleted entries also use diff-by-keys-find-deleted.

Example:
    (diff-by-keys (pk.sym :a 1
                          :b 2)
                  (pk.sym :a 1
                          :b 2
                          :c 3))
    => (dict :c 3)

Descends through nested keyed containers, looking for changes.  Nodes of
returned keyed object are the changes or new entries.  Use diffs-in-conflict-p
to determine if two diffs have conflicting changes."
  (let ((result (diff-by-keys-internal a b :test test)))
    (if result
        (mutable-dict-finalize result)
        nil)))

(defun diff-by-keys-internal (a b &key (test #'equal) empty-result)
  (let ((a-has-keys (and (settable-keyed-p a) (plusp (lengthk a))))
        (b-has-keys (and (settable-keyed-p b) (plusp (lengthk b)))))
    (cond ((and a-has-keys b-has-keys)
           ;; Both have keys, must recurse
           (flet ((reducer (acc key)
                    (let ((res (diff-by-keys-internal (diff-getk a key)
                                                      (diff-getk b key)
                                                      :test test
                                                      :empty-result 'no-result)))
                      (if (and res (not (eq res 'no-result)))
                          (diff-setk acc key res)
                          acc))))
             (let* ((md (mutable-dict))
                    (obj (reduce-keys-union #'reducer md a b)))
               (if (and (eq md obj) (zerop (lengthk md)))
                   empty-result
                   obj))))
          ;; If no b-keys, then b
          (a-has-keys b)
          ;; If no a-keys, then b is new or replaced a
          (b-has-keys b)
          ;; Neither have keys
          ((funcall test a b)
           empty-result)
          (t
           b))))

(defun diff-by-keys-find-deleted (a b &key (test #'equal)
                                        (deleted-value :deleted))
  "Diff two keyed objects, finding new, changed and deleted entries.

Nodes that contain :DELETED (or DELETED-VALUE if overridden) were removed from
B.

Example:
    (diff-by-keys-find-deleted (pk.sym :a 1
                                    :b 2
                                    :c 3)
                            (pk.sym :a 1
                                    :b 2))
    => (pk.sym :c :deleted)

Descends through nested keyed containers, looking for changes.  Nodes of
returned keyed object are the changes or new entries.  Use diffs-in-conflict-p
to determine if two diffs have conflicting changes."
  (let ((result (diff-by-keys-find-deleted-internal a b
                                                    :test test
                                                    :deleted-value deleted-value)))
    (if result
        (mutable-dict-finalize result)
        nil)))

(defun diff-by-keys-find-deleted-internal (a b &key (test #'equal)
                                                 empty-result
                                                 (deleted-value :deleted))
  (let ((a-has-keys (and (settable-keyed-p a) (plusp (lengthk a))))
        (b-has-keys (and (settable-keyed-p b) (plusp (lengthk b)))))
    (cond ((and a-has-keys b-has-keys)
           ;; Both have keys, must recurse
           (flet ((reducer (acc key)
                    (let ((res (diff-by-keys-find-deleted-internal (diff-getk a key)
                                                                   (diff-getk b key)
                                                                   :test test
                                                                   :empty-result 'no-result
                                                                   :deleted-value deleted-value)))
                      (if (and res (not (eq res 'no-result)))
                          (diff-setk acc key res)
                          acc))))
             (let* ((md (mutable-dict))
                    (obj (reduce-keys-union #'reducer md a b)))
               (if (and (eq md obj) (zerop (lengthk md)))
                   empty-result
                   obj))))
          ;; If no b-keys, then b or :deleted
          (a-has-keys (or b deleted-value))
          ;; If no a-keys, then b is new or replaced a
          (b-has-keys b)
          ;; Neither have keys
          ((and (settable-keyed-p a) (settable-keyed-p b))
           ;; but they are both keyed objects, and both are empty
           empty-result)
          ((funcall test a b)
           empty-result)
          (t (or b deleted-value)))))

(defun diffs-in-conflict-p (diff-a diff-b &key (test #'equal))
  "Determine if DIFF-A and DIFF-B can not be applied without overwritting the
change from the other."
  ;; Walk tree, return t if any nodes differ
  (fold-keyed diff-a
              (lambda (acc key a-value)
                (declare (ignore acc))
                (let ((b-value (diff-getk diff-b key 'not-found)))
                  (unless (eq 'not-found b-value)
                    (let ((a-is-node (not (settable-keyed-p a-value)))
                          (b-is-node (not (settable-keyed-p b-value))))
                      (cond ((and a-is-node b-is-node)
                             (unless (funcall test a-value b-value)
                               (return-from diffs-in-conflict-p t)))
                            ((or a-is-node b-is-node)
                             ;; One is node, other is branch
                             (return-from diffs-in-conflict-p t))
                            (t  ;; Both are branches
                             (when (diffs-in-conflict-p a-value b-value)
                               (return-from diffs-in-conflict-p t)))))))))
  nil)

(defun apply-diff (tree diff &key (deleted-value :deleted))
  "Apply a diff to a tree and return the changed tree."
  (fold-keyed diff
              (lambda (new-tree diff-key diff-value)
                (cond ((eq diff-value deleted-value)
                       (diff-remk new-tree diff-key))
                      ((settable-keyed-p diff-value)
                       ;; Recurse tree, or replace node?
                       (let ((new-tree-value (diff-getk new-tree diff-key 'not-found)))
                         (cond ((not (settable-keyed-p new-tree-value))
                                (eq new-tree-value 'not-found)
                                ;; Nothing matching this in original tree, we can dump the diff
                                (diff-setk new-tree diff-key diff-value))
                               ((settable-keyed-p diff-value)
                                (diff-setk new-tree diff-key (apply-diff new-tree-value diff-value)))
                               (t
                                ;; Replace node with tree
                                (diff-setk new-tree diff-key diff-value)))))
                      (t
                       ;; diff-value is a node.  Replace key in tree with node.
                       (diff-setk new-tree diff-key diff-value))))
              tree))

(defun keyeds-equal-p (keyed-a keyed-b &key (test #'equal))
  "Compare KEYED-A and KEYED-B to see if any nodes differ."
  (not (diff-by-keys-find-deleted keyed-a keyed-b :test test)))

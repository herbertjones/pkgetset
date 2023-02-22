(uiop:define-package #:pkgetset/set-by-key
  (:use #:cl
        #:pkgetset/interfaces
        #:pkgetset/get-by-key)
  (:import-from #:serapeum
                #:->)
  (:export #:setk!
           #:setk*
           #:setk*!
           #:modk
           #:modk!
           #:modk*
           #:modk*!
           #:emodk
           #:emodk!
           #:emodk*
           #:emodk*!
           #:remk!
           #:remk*
           #:remk*!
           #:*setk-container-override*
           #:keyed-merge)
  (:documentation "Convenience functions for setting settable-keyed containers."))
(in-package #:pkgetset/set-by-key)

(defparameter *setk-container-override* nil
  "Override default container when making new nodes using setk*.")

(defmacro setk! (settable key value)
  "Set KEY in SETTABLE to VALUE and replace SETTABLE with updated container.

Does not modify the original container.

Example:
    (let ((rec (pk.sym :b 2 :c 3)))
       (setk! rec :a 1)
       rec)
    => (pk.sym :a 1
               :b 2
               :c 3)"
  `(setf ,settable (setk ,settable ,key ,value)))

(-> setk* (settable-keyed list t) settable-keyed)
(defun setk* (container path value)
  "Update PATH of CONTAINER with VALUE.

When containers have to be created, the same type of container is used.  To
override this use *SETK-CONTAINER-OVERRIDE*.

Example:
  (let ((rec (pk.sym :a 1
                     :b (pk.sym :ba 10))))
    (setk* rec '(:b :bb) 20))
  => (pk.sym :a 1
             :b (pk.sym :ba 10 :bb 20))"
  (cond ((cdr path) ;; More path parts remain
         (values (setk container
                       (car path)
                       (setk* (getk container (car path) (or *setk-container-override*
                                                             (settable-keyed->empty container)))
                              (cdr path)
                              value))))
        ((car path)
         (values (setk container (car path) value)))
        (t
         (error "Empty path passed to setk*."))))

(defmacro setk*! (settable path value)
  "Set node at PATH in SETTABLE to VALUE and replace SETTABLE with updated
container.

When containers have to be created, the same type of container is used.  To
override this use *SETK-CONTAINER-OVERRIDE*.

Example:
    (let ((obj (pk.sym :a 1
                       :b (pk.sym :bb 2))))
        (setk*! obj '(:b :bb) 3))
    => (pk.sym :a 1 :b (pk.sym :bb 3))"
  `(setf ,settable (setk* ,settable ,path ,value)))

(-> modk (settable-keyed t (-> (t) t) &optional t) (values (satisfies keyedp) &optional))
(defun modk (settable key adjuster &optional default)
  "Adjust a value on settable-keyed container by KEY.  ADJUSTER takes old value or
DEFAULT as argument, returns new value.

Example:
    (let ((obj (pk.sym :a 1
                       :b 2)))
        (modk obj :b #'1+))
    => (pk.sym :a 1 :b 3)"
  (let ((previous-value (getk settable key 'not-set)))
    (setk settable key (funcall adjuster (if (eq previous-value 'not-set) default previous-value)))))

(-> modk* (settable-keyed list (-> (t) t) &optional t) settable-keyed)
(defun modk* (settable path adjuster &optional default)
  "Adjust a value on settable-keyed container by PATH list.  ADJUSTER takes old value or
DEFAULT as argument, returns new value.

Example:
    (let ((obj (pk.sym :a 1
                       :b (pk.sym :ba 10))))
        (modk* obj '(:b :bb) #'1+ 0))
    => (pk.sym :a 1 :b (pk.sym :ba 10 :bb 1))"
  (let ((previous-value (getk* settable path 'not-set)))
    (setk* settable path (funcall adjuster (if (eq previous-value 'not-set) default previous-value)))))

(-> emodk (settable-keyed t (-> (t) t)) settable-keyed)
(defun emodk (settable key adjuster)
  "Adjust a value on settable-keyed container by KEY.  ADJUSTER takes old value
and returns new value.  Signals MISSING-KEY if key missing.

Example:
    (let ((obj (pk.sym :a 1
                       :b 2)))
        (emodk obj :c #'1+))
    => [Condition of type missing-key]"
  (values (setk settable key (funcall adjuster (egetk settable key)))))

(-> emodk* (settable-keyed list (-> (t) t)) settable-keyed)
(defun emodk* (settable path adjuster)
  "Adjust a value on settable-keyed container by PATH list.  ADJUSTER takes old value and
returns new value.  Signals MISSING-KEY if path missing.

Example:
    (let ((obj (pk.sym :a 1
                       :b 2)))
        (emodk* obj '(:b) #'1+))
    => (pk.sym :a 1 :b 3)"
  (setk* settable path (funcall adjuster (egetk* settable path))))

(defmacro modk! (settable key adjuster &optional default)
  "Adjust a value on settable-keyed container by KEY in place.  ADJUSTER takes old
value or DEFAULT as argument, returns new value.

Example:
    (let ((obj (pk.sym :a 1
                       :b 2)))
        (modk! obj :b #'1+)
        obj)
    => (pk.sym :a 1 :b 3)"
  `(setf ,settable (modk ,settable ,key ,adjuster ,default)))

(defmacro modk*! (settable path adjuster &optional default)
  "Adjust a value on settable-keyed container by PATH list in place.  ADJUSTER takes old
value or DEFAULT as argument, returns new value.

Example:
    (let ((obj (pk.sym :a 1
                       :b (pk.sym :ba 10))))
        (modk*! obj '(:b :bb) #'1+ 0)
        obj)
    => (pk.sym :a 1 :b (pk.sym :ba 10 :bb 1))"
  `(setf ,settable (modk* ,settable ,path ,adjuster ,default)))

(defmacro emodk! (settable key adjuster)
  "Adjust a value on settable-keyed container by KEY in place.  ADJUSTER takes old value and
returns new value.  Errors if key missing.

Example:
    (let ((obj (pk.sym :a 1
                       :b 2)))
        (emodk! obj :b #'1+)
        obj)
    => (pk.sym :a 1 :b 3)"
  `(setf ,settable (emodk ,settable ,key ,adjuster)))

(defmacro emodk*! (settable path adjuster)
  "Adjust a value on settable-keyed container by PATH list in place.  ADJUSTER
takes old value and returns new value.  Errors if path missing.

Example:
    (let ((obj (pk.sym :a 1
                       :b 2)))
        (emodk*! obj '(:b) #'1+)
        obj)
    => (pk.sym :a 1 :b 3)"
  `(setf ,settable (emodk* ,settable ,path ,adjuster)))

(defmacro remk! (settable key)
  "Unset a value on settable-keyed container by KEY in place

Example:
    (let ((obj (pk.sym :a 1
                       :b 2)))
        (remk! obj :b)
        obj)
    => (pk.sym :a 1)"
  `(setf ,settable (remk ,settable ,key)))

(-> remk* (settable-keyed list) settable-keyed)
(defun remk* (settable path)
  "Unset a value on settable-keyed container by path list

Example:
    (let ((obj (pk.sym :a 1
                       :b (pk.str \"nested\" t))))
        (remk* obj '(:b \"nested\")))
    => (pk.sym :a 1 :b (pk.str))"
  (cond ((cdr path) ;; More path parts remain
         (values (setk settable
                       (car path)
                       (remk* (getk settable (car path) (or *setk-container-override*
                                                            (settable-keyed->empty settable)))
                              (cdr path)))))
        ((car path)
         (values (remk settable (car path))))
        (t
         (error "Empty path passed to remk*."))))

(defmacro remk*! (settable path)
  "Unset a value on settable-keyed container by PATH list in place.

Example:
    (let ((obj (pk.sym :a 1
                       :b (pk.str \"nested\" t))))
        (remk*! obj '(:b \"nested\"))
        obj)
    => (pk.sym :a 1 :b (pk.str))"
  `(setf ,settable (remk* ,settable ,path)))

(defun keyed-merge (&rest keyed-rest)
  "Merge the keyed objects together.  Earlier keyed replaces items in the
later when keys collide.

Example:
    (keyed-merge (pk.sym :a 1
                         :b 2)
                 (pk.sym :b 20
                         :c 30))
    => (pk.sym :a 1 :b 2 :c 30)"
  (let ((len (length keyed-rest)))
    (cond ((zerop len)
           nil)
          ((= 1 len)
           (car keyed-rest))
          (t
           (reduce #'keyed-merge-2
                   (cdr keyed-rest)
                   :initial-value (car keyed-rest))))))

(defun keyed-merge-2 (keyed-a keyed-b)
  "(package internal) Merge the keyed together.  The first keyed replaces items in
the second when keys collide."
  (fold-keyed keyed-a (lambda (acc key val)
                        (setk acc key val))
              keyed-b))

(uiop:define-package #:pkgetset/get-by-key
  (:use)
  (:mix #:pkgetset/interfaces/keyed-container
        #:pkgetset/interfaces/get-by-key
        #:cl)
  (:import-from #:pkgetset/util
                #:over-pairs)
  (:import-from #:serapeum
                #:->)
  (:export #:egetk
           #:egetk*
           #:ewith-keyed
           #:getk*
           #:getk?
           #:getk*?
           #:missing-key
           #:missing-key-description
           #:missing-key-key
           #:missing-key-keyed
           #:over-keyed
           #:with-keyed)
  (:documentation "Helpers to access a keyed container by key"))
(in-package #:pkgetset/get-by-key)

(-> getk? (keyed t) boolean)
(defun getk? (keyed key)
  "Return T if KEYED container has the entry KEY.

Example:
    (let ((obj (pk.sym :a 1
                       :b 2)))
      (getk? obj :b))
    => t"
  (not (eq (getk keyed key 'no-key) 'no-key)))

(-> getk*? (keyed list) boolean)
(defun getk*? (keyed path)
  "Return T if KEYED container has a node at PATH.

Example:
    (let ((obj (pk.sym :a 1
                       :b (pk.sym :bb 2))))
      (getk*? obj '(:b :bb)))
    => t"
  (not (eq (getk* keyed path 'no-key) 'no-key)))

(define-condition missing-key (error)
  ((key :reader missing-key-key
        :initarg :key
        :documentation "The key that was missing.")
   (keyed :reader missing-key-keyed
          :initarg :keyed
          :documentation "The keyed data")
   (description :reader missing-key-description
                :initarg :description
                :type string
                :documentation "Explanation of what happened."))
  (:documentation "Signaled when a key is missing."))

(-> egetk (keyed t) t)
(defun egetk (keyed key)
  "Return node referenced by KEY from KEYED.  Signals MISSING-KEY error if KEY
missing.

Example:
    (let ((obj (pk.sym :a 1
                       :b 2)))
      (egetk obj :b))
    => 2"
  (let ((value (getk keyed key 'missing)))
    (when (eq value 'missing)
      (error 'missing-key
             :key key
             :keyed keyed
             :description (format nil "Missing ~s in ~s" key keyed)))
    value))

(-> getk* (keyed list &optional t) t)
(defun getk* (container path &optional default)
  "Return node referenced by PATH from KEYED.  Returns DEFAULT if no node exists at
PATH.

(let ((obj (pk.sym :a 1
                   :b (pk.sym :bb 2))))
  (getk* obj '(:b :bb)))
=> 2
"
  (loop for ptr = container then next-ptr
        for path-part in path
        for next-ptr = (getk ptr path-part)
        unless next-ptr
          do (return default)
        finally (return (or next-ptr default))))

(-> egetk* (keyed list) t)
(defun egetk* (container path)
  "Return node referenced by PATH from KEYED.  Signals MISSING-KEY error if node at PATH
is missing.

Example:
    (let ((obj (pk.sym :a 1
                       :b (pk.sym :bb 2))))
      (egetk* obj '(:b :bb)))
    => 2"
  (loop for ptr = container then next-ptr
        for path-part in path
        for rev-path-list = (list path-part) then (cons path-part rev-path-list)
        for next-ptr = (getk ptr path-part 'missing)
        when (eq next-ptr 'missing)
          do (let* ((keys (nreverse rev-path-list)))
               (error 'missing-key
                      :key (car keys)
                      :keyed ptr
                      :description (format nil "Missing ~s at ~s" keys ptr)))
        finally (return next-ptr)))

;; with-keyed helpers
(eval-when (:compile-toplevel :load-toplevel :execute)
  (-> unwrap-paths-to-symbols (list) list)
  (defun unwrap-paths-to-symbols (paths-to-symbols)
    "(macro internal) Unroll possibly nested string to symbol pairs to symbols to
their full paths."
    (let ((symbols-to-paths '()))
      (labels ((process-data (data path)
                 (over-pairs data (key value)
                   (etypecase value
                     (cons (process-data value (cons key path)))
                     (symbol (push (cons value (reverse (cons key path))) symbols-to-paths))))))
        (process-data paths-to-symbols '()))
      (sort symbols-to-paths (lambda (a b)
                               (< (length a) (length b))))))

  (-> path-to-symbols-to-get-exp (t list &key (:error-if-not-exist boolean)) list)
  (defun path-to-symbols-to-get-exp (data paths-to-symbols &key error-if-not-exist)
    "(macro internal) Unroll possibly nested string to symbol pairs to use with let*."
    (let ((symbols-to-paths (unwrap-paths-to-symbols paths-to-symbols))
          (nested-paths-to-symbols (make-hash-table :test #'equal))
          (let-list '())
          (get-function (if error-if-not-exist 'egetk 'getk)))
      ;; Prepare nested assignments
      (setf (gethash '() nested-paths-to-symbols) data)
      (dolist (row symbols-to-paths)
        (let ((full-path (cdr row)))
          (loop :for path-part :in (butlast full-path)
                :for rpath := (list path-part) :then (cons path-part rpath)
                :for path = (reverse rpath)
                :do (unless (gethash path nested-paths-to-symbols)
                      (let ((new-sym (gensym))
                            (parent-sym (gethash (butlast path) nested-paths-to-symbols)))
                        (push (list new-sym
                                    (list get-function parent-sym path-part))
                              let-list)
                        (setf (gethash path nested-paths-to-symbols) new-sym))))))
      ;; Prepare non-nested assignments
      (dolist (row symbols-to-paths)
        (let ((sym (car row))
              (full-path (cdr row)))
          (push (list sym
                      (list get-function
                            (gethash (butlast full-path) nested-paths-to-symbols :error)
                            (car (last full-path))))
                let-list)))
      (nreverse let-list))))

(defmacro with-keyed (keyed (&rest paths-to-symbols) &body body)
  "Bind path pairs from paths-to-symbols to symbols within.  Nesting is allowed.

Example:
    (with-keyed data (\"a\" a
                      \"b\" (\"ba\" ba
                             \"bb\" (\"bba\" bba)))
              (list a ba bba))"
  (let ((data (gensym "keyed")))
    `(let* ((,data ,keyed)
            ,@(path-to-symbols-to-get-exp data paths-to-symbols))
       ,@body)))

(defmacro ewith-keyed (keyed (&rest paths-to-symbols) &body body)
  "Bind path pairs from paths-to-symbols to symbols within.  Nesting is allowed.
Signals MISSING-KEY if key doesn't exist.

Example:
    (ewith-keyed data (\"a\" a
                       \"b\" (\"ba\" ba
                              \"bb\" (\"bba\" bba)))
              (list a ba bba))"
  (let ((data (gensym "keyed")))
    `(let* ((,data ,keyed)
            ,@(path-to-symbols-to-get-exp data paths-to-symbols
                                          :error-if-not-exist t))
       ,@body)))

(defmacro over-keyed ((keyed &rest arg-plist) &body body)
  "Iterate over KEYED.  Bindings on key, value and accumulator set using ARG-PLIST.

ARG-PLIST can have the forms:
  :key KEY
  :value VALUE
  :accumulator PREVIOUS-RESULT
  :initial FIRST-ACCUMULATOR

Example:
    (over-keyed (kdata :key k)
      (format t \"Key: ~A~%\" k))"
  (let ((key (or (getf arg-plist :key) (gensym "key")) )
        (value (or (getf arg-plist :value) (gensym "value")))
        (acc (or (getf arg-plist :accumulator) (gensym "accumulator"))))
    `(fold-keyed ,keyed
                 (lambda (,acc ,key ,value)
                   (declare (ignorable ,acc ,key ,value))
                   ,@body)
                 ,(getf arg-plist :initial))))

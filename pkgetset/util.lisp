(uiop:define-package #:pkgetset/util
  (:use #:cl)
  (:export
   #:over-pairs)
  (:documentation "Internal utility functions"))
(cl:in-package #:pkgetset/util)

(defmacro over-pairs (pair-list (key-name val-name) &body body)
  "Iterate over a loose set of paired data, where PAIR-LIST is a list of key
 value key value...  Like a plist, but keys do not have to be unique.

Example:
    (over-pairs '(:a :b :c :d :e :f) (key val)
      (format t \"(~a . ~a) \" key val))
  Prints: (A . B) (C . D) (E . F)"
  (let ((data (gensym)))
    `(loop for ,data = ,pair-list then (cddr ,data)
           while ,data
           for ,key-name = (car ,data)
           for ,val-name = (cadr ,data)
           do (progn ,@body))))

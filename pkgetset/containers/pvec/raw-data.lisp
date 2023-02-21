(uiop:define-package #:pkgetset/containers/pvec/raw-data
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:export
   #:raw-id++
   #:raw-id--

   #:raw-data
   #:make-raw-data
   #:raw-data-p
   #:copy-raw-data
   #:raw-data-id
   #:raw-data-value)
  (:documentation "Internal data of pvec and associated functions"))
(cl:in-package #:pkgetset/containers/pvec/raw-data)

(defstruct raw-data
  "Data used inside of the tree-set of the pvec struct.

Contains an ID that is a list of fixnums and a value that can be anything to store."
  (id '() :type list)
  (value nil :type t))

(-> raw-id++ (list) list)
(defun raw-id++ (raw-id)
  (list (1+ (car raw-id))))

(-> raw-id-- (list) list)
(defun raw-id-- (raw-id)
  (list (1- (car raw-id))))

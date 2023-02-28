(uiop:define-package #:pkgetset/build-keyed
  (:use)
  (:import-from #:serapeum
                #:->)
  (:mix #:pkgetset/interfaces
        #:pkgetset/get-by-key
        #:cl)
  (:export
   #:build-keyed)
  (:documentation "Package to convert non-persistent types to data types supported by this library."))
(cl:in-package #:pkgetset/build-keyed)

(-> build-keyed
  ((or hash-table string cons vector keyed)
   (-> () keyed)
   (-> (keyed t keyed) keyed)
   (-> () keyed)
   (-> (keyed t) keyed)
   boolean)
  keyed)
(defun build-keyed (data-to-convert
                    empty-persistent-keyed key-value-adder
                    empty-persistent-unkeyed unkeyed-appender
                    list-are-pairs)
  "Convert data to (persistent) keyed types using arguments as base.

Used to convert data from sources like converted JSON and map it to settable-keyed types.

    (build-keyed json
                #'empty-pk.str
                #'setk
                #'empty-pvec
                #'pvec-push
                nil)"
  (flet ((recurse (next-data)
           (build-keyed-inner next-data
                              empty-persistent-keyed key-value-adder
                              empty-persistent-unkeyed unkeyed-appender
                              list-are-pairs)))
    (typecase data-to-convert
      (hash-table (let ((pd (funcall empty-persistent-keyed)))
                    (maphash #'(lambda (key raw-val)
                                 (setf pd (funcall key-value-adder pd key (recurse raw-val))))
                             data-to-convert)
                    pd))
      (vector (let ((pd (funcall empty-persistent-unkeyed)))
                (loop :for raw-val :across data-to-convert
                      :do (setf pd (funcall unkeyed-appender pd (recurse raw-val))))
                pd))
      (keyed data-to-convert)
      (t (error "Unexpected data: ~S of type ~A"
                data-to-convert
                (type-of data-to-convert))))))

(defun build-keyed-inner (data-to-convert
                          empty-persistent-keyed key-value-adder
                          empty-persistent-unkeyed unkeyed-appender
                          list-are-pairs)
  "Convert data to persistent types.

   Used by build-keyed to iterate over types that may or may not be recursed upon."
  (flet ((recurse (next-data)
           (build-keyed-inner next-data
                              empty-persistent-keyed key-value-adder
                              empty-persistent-unkeyed unkeyed-appender
                              list-are-pairs)))
    (typecase data-to-convert
      (null nil)
      (hash-table (let ((pd (funcall empty-persistent-keyed)))
                    (maphash #'(lambda (key raw-val)
                                 (setf pd (funcall key-value-adder pd key (recurse raw-val))))
                             data-to-convert)
                    pd))
      (string data-to-convert)
      (cons (if list-are-pairs
                (let ((pd (funcall empty-persistent-keyed)))
                  (loop :for data := data-to-convert :then (cddr data)
                        :while data
                        :for key := (car data)
                        :for raw-val := (cadr data)
                        :do (setf pd (funcall key-value-adder pd key (recurse raw-val))))
                  pd)
                (let ((pd (funcall empty-persistent-unkeyed)))
                  (loop :for raw-val :in data-to-convert
                        :do (setf pd (funcall unkeyed-appender pd (recurse raw-val))))
                  pd)))
      (vector (let ((pd (funcall empty-persistent-unkeyed)))
                (loop :for raw-val :across data-to-convert
                      :do (setf pd (funcall unkeyed-appender pd (recurse raw-val))))
                pd))
      (number data-to-convert)
      (keyed data-to-convert)
      (t (error "Unexpected data: ~S of type ~A"
                data-to-convert
                (type-of data-to-convert))))))

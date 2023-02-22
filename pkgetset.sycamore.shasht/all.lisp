(uiop:define-package #:pkgetset.sycamore.shasht
  (:nicknames #:pkgetset.sycamore.shasht/all)
  (:use #:cl
        #:pkgetset
        #:pkgetset.sycamore)
  (:import-from #:pkgetset.sycamore/sycamore-tree-map
                #:wrapped-tree-map-inner)
  (:import-from #:shasht
                #:read-json*
                #:print-json-value
                #:print-json-key-value
                #:with-json-array
                #:with-json-object)
  (:import-from #:sycamore
                #:fold-tree-map)
  (:import-from #:str)
  (:import-from #:serapeum
                #:->)
  (:export #:json->pk.gen
           #:json->pk.str
           #:json->pk.sym))
(in-package #:pkgetset.sycamore.shasht/all)

(-> json->pk.gen (string) pk.gen)
(defun json->pk.gen (json-str)
  "Create a generic tree-map from JSON."
  (let ((json (read-json* :stream json-str
                          :array-format :vector
                          :object-format :hash-table)))
    (values (build-keyed json
                         #'empty-pk.gen
                         #'setk
                         #'empty-pvec
                         #'pvec-push
                         nil))))

(-> json->pk.str (string) pk.str)
(defun json->pk.str (json-str)
  "Create a string key tree-map from JSON."
  (let ((json (read-json* :stream json-str
                          :array-format :vector
                          :object-format :hash-table)))
    (values (build-keyed json
                         #'empty-pk.str
                         #'setk
                         #'empty-pvec
                         #'pvec-push
                         nil))))

(-> json->pk.sym-key-transformer (string) keyword)
(defun json->pk.sym-key-transformer (key)
  "Convert the string key to an uppercase keyword."
  (intern (str:upcase key) 'keyword))

(-> json->pk.sym (string
                  &key (:key-transformer (-> (string) keyword)))
  pk.sym)
(defun json->pk.sym (json-str
                     &key (key-transformer #'json->pk.sym-key-transformer))
  "Create a symbol key tree-map from JSON.

Possibly a bad idea unless you know what the keys are, as otherwise you may
overpopulate the keyword symbol table, or whichever symbol table
key-transformer uses."
  (let ((json (read-json* :stream json-str
                          :array-format :vector
                          :object-format :hash-table)))
    (flet ((transforming-setk (settable key value)
             (setk settable
                   (funcall key-transformer key)
                   value)))
      (values (build-keyed json
                           #'empty-pk.sym
                           #'transforming-setk
                           #'empty-pvec
                           #'pvec-push
                           nil)))))

(-> generic-key-fix ((or symbol string number)) (or string number))
(defun generic-key-fix (key)
  "Convert the key into something nicer if needed."
  (values (etypecase key
            (symbol (str:downcase (symbol-name key)))
            (string key)
            (number key))))

(defmethod print-json-value ((value pk.gen) output-stream)
  (with-json-object output-stream
    (fold-tree-map
     (lambda (acc key val)
       (declare (ignore acc))
       (print-json-key-value value (generic-key-fix key) val output-stream))
     nil
     (wrapped-tree-map-inner value)))
  value)

(defmethod print-json-value ((value pk.sym) output-stream)
  (with-json-object output-stream
    (fold-tree-map
     (lambda (acc key val)
       (declare (ignore acc))
       (print-json-key-value value (str:downcase (symbol-name key)) val output-stream))
     nil
     (wrapped-tree-map-inner value)))
  value)

(defmethod print-json-value ((value pk.str) output-stream)
  (with-json-object output-stream
    (fold-tree-map
     (lambda (acc key val)
       (declare (ignore acc))
       (print-json-key-value value key val output-stream))
     nil
     (wrapped-tree-map-inner value)))
  value)

(defmethod print-json-value ((value pvec) output-stream)
  (with-json-array output-stream
    (fold-keyed value
                (lambda (acc key val)
                  (declare (ignore acc key))
                  (print-json-value val output-stream))))
  value)

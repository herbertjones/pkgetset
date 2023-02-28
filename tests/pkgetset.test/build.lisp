(uiop:define-package #:pkgetset.test/build
  (:use)
  (:import-from #:serapeum
                #:dict)
  (:mix #:parachute
        #:pkgetset
        #:cl))
(cl:in-package #:pkgetset.test/build)

(defun pdict-push (pdict new-value)
  "Use the pdict as a terrible vector.  For testing."
  (loop :for i :from 0
        :unless (getk? pdict i)
          :do (return (setk pdict i new-value))))

(defun build-basic (raw-data &optional lists-are-paired)
  (build-keyed raw-data
               #'empty-pdict
               #'setk
               #'empty-pdict
               #'pdict-push
               lists-are-paired))

(define-test "build-keyed basic types"
  (let ((data (build-basic (dict :a 1
                                 :b 2
                                 :c nil))))
    (is = 1 (getk data :a))
    (is = 2 (getk data :b))
    (is eql nil (getk data :c))))

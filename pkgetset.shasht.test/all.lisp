(uiop:define-package #:pkgetset.shasht.test
  (:nicknames #:pkgetset.shasht.test/all)
  (:use)
  (:mix #:parachute
        #:pkgetset
        #:pkgetset.sycamore
        #:pkgetset.shasht
        #:shasht
        #:arrow-macros
        #:cl))
(cl:in-package #:pkgetset.shasht.test)
(named-readtables:in-readtable :interpol-syntax)

(define-test "json->pk.str"
  (let* ((tm (json->pk.str (write-json* (-> (empty-pk.str)
                                          (setk* '("a" "aa" "aac") "aacd")
                                          (setk* '("a" "aa" "aad") "aada"))
                                        :stream nil))))
    (is equal "aacd" (getk* tm '("a" "aa" "aac")))
    (is equal "aada" (getk* tm '("a" "aa" "aad"))))

  (let* ((tm (json->pk.str #?<{"a":{"aa":{"aac":"aacd","aad":"aada"}}}>)))
    (is equal "aacd" (getk* tm '("a" "aa" "aac")))
    (is equal "aada" (getk* tm '("a" "aa" "aad")))))

(define-test "json->pk.sym"
  (let* ((tm (json->pk.sym (write-json* (-> (empty-pk.sym)
                                          (setk* '(:a :aa :aac) "aacd")
                                          (setk* '(:a :aa :aad) "aada"))
                                        :stream nil))))
    (is equal "aacd" (getk* tm '(:a :aa :aac)))
    (is equal "aada" (getk* tm '(:a :aa :aad))))

  (let* ((tm (json->pk.sym #?<{"a":{"aa":{"aac":"aacd","aad":"aada"}}}>)))
    (is equal "aacd"
        (getk* tm '(:a :aa :aac)))
    (is equal "aada"
        (getk* tm '(:a :aa :aad)))))

(define-test "json->pk.gen"
  (let* ((tm (json->pk.gen (write-json* (-> (empty-pk.gen)
                                          (setk* '("a" "aa" "aac") "aacd")
                                          (setk* '("a" "aa" "aad") "aada"))
                                        :stream nil))))
    (is equal "aacd" (getk* tm '("a" "aa" "aac")))
    (is equal "aada" (getk* tm '("a" "aa" "aad"))))

  (let* ((tm (json->pk.gen #?<{"a":{"aa":{"aac":"aacd","aad":"aada"}}}>)))
    (is equal "aacd" (getk* tm '("a" "aa" "aac")))
    (is equal "aada" (getk* tm '("a" "aa" "aad")))))

(define-test "pvec"
  (let* ((tm (json->pk.gen "{\"a\":[1, 2, 3]}")))
    (of-type pvec (getk* tm '("a")))
    (is equal 3 (getk* tm '("a" 2)))))

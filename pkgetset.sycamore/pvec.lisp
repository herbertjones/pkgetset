(uiop:define-package #:pkgetset.sycamore/pvec
  (:mix #:cl
        #:pkgetset.sycamore/pvec/get
        #:pkgetset.sycamore/pvec/set
        #:pkgetset.sycamore/pvec/pvec
        #:pkgetset.sycamore/pvec/diff)
  (:export #:pvec
           #:empty-pvec
           #:pvec-push
           #:list->pvec
           #:sequence->pvec
           #:pvec-insert-at-pos)
  (:documentation "A persistent vector structure with getk and setk implementation

With setk, pvec can accept some additional special keys to prevent replacing
values.

:start
  Insert value at start of pvec.

:end
  Insert value at end of pvec.

(:insert pos)
  Insert value at position."))
(cl:in-package #:pkgetset.sycamore/pvec)

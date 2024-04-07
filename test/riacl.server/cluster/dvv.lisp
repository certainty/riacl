(in-package :riacl.server.tests.cluster)

(serapeum:defconst +vnode-1+ "urn:x-riacl:10:a:1")
(serapeum:defconst +vnode-2+ "urn:x-riacl:10:a:2" )
(serapeum:defconst +vnode-3+ "urn:x-riacl:10:a:3")
(serapeum:defconst +vnode-4+ "urn:x-riacl:10:a:4")
(serapeum:defconst +vnode-5+ "urn:x-riacl:10:a:5")

(define-test examples-of-dotted-version-vector ()
  "This is an example test showing the usage of dotted version vectors"
  (let ((a (dvv:make-dotted-version-vector))
        (b (dvv:make-dotted-version-vector)))
    (assert-true t)))

(define-test merge-works ()
  (let* ((c1 (dvv:make-dotted-version-vector
              :initial-history (list
                                (dvv:dot +vnode-1+ :counter 1 :timestamp 1)
                                (dvv:dot +vnode-2+ :counter 2 :timestamp 2))
              :initial-dot (dvv:dot +vnode-4+ :counter 4 :timestamp 4)))
         (c2 (dvv:make-dotted-version-vector
              :initial-history (list
                                (dvv:dot +vnode-3+ :counter 3 :timestamp 3)
                                (dvv:dot +vnode-4+ :counter 3 :timestamp 3)))))

    (assert-true (dvv:emptyp (dvv:merge* (dvv:make-dotted-version-vector))) "Fresh dvv merges to empty dvv")

    (let ((c1-merged-with-c2 (dvv:merge* c1 c2)))
      (assert-true (null (dvv:dotted-version-vector-dot c1-merged-with-c2)))

      (let ((history (mapcar #'prelude:to-plist
                             (dvv:dotted-version-vector-history-sorted c1-merged-with-c2))))
        (assert-equal
         `((:actor-id ,+vnode-1+ :counter 1 :timestamp 1)
           (:actor-id ,+vnode-2+ :counter 2 :timestamp 2)
           (:actor-id ,+vnode-3+ :counter 3 :timestamp 3)
           ;; event 3 3 is removed for vnode-4
           (:actor-id ,+vnode-4+ :counter 4 :timestamp 4))
         history)))))

(define-test merge-less-left ()
  (let* ((c1 (dvv:make-dotted-version-vector :initial-dot (dvv:dot +vnode-1+ :counter 1 :timestamp 1)))
         (c2 (dvv:make-dotted-version-vector :initial-history
                                             (list (dvv:dot +vnode-2+ :counter 2 :timestamp 2))
                                             :initial-dot (dvv:dot +vnode-3+ :counter 3 :timestamp 3)))
         (c1+c2 (dvv:merge* c1 c2)))
    (assert-true (null (dvv:dotted-version-vector-dot c1+c2)))
    (assert-equal
     `((:actor-id ,+vnode-1+ :counter 1 :timestamp 1)
       (:actor-id ,+vnode-2+ :counter 2 :timestamp 2)
       (:actor-id ,+vnode-3+ :counter 3 :timestamp 3))
     (mapcar #'prelude:to-plist (dvv:dotted-version-vector-history-sorted c1+c2)))))

(define-test merge-less-right ()
  (let* ((c1 (dvv:make-dotted-version-vector :initial-history (list (dvv:dot +vnode-2+ :counter 2 :timestamp 2))
                                             :initial-dot (dvv:dot +vnode-3+ :counter 3 :timestamp 3)))
         (c2 (dvv:make-dotted-version-vector :initial-dot (dvv:dot +vnode-1+ :counter 1 :timestamp 1)))
         (c1+c2 (dvv:merge* c1 c2)))
    (assert-true (null (dvv:dotted-version-vector-dot c1+c2)))
    (assert-equal
     `((:actor-id ,+vnode-1+ :counter 1 :timestamp 1)
       (:actor-id ,+vnode-2+ :counter 2 :timestamp 2)
       (:actor-id ,+vnode-3+ :counter 3 :timestamp 3))
     (mapcar #'prelude:to-plist (dvv:dotted-version-vector-history-sorted c1+c2)))))

(define-test merge-same-id ()
  (let* ((c1 (dvv:make-dotted-version-vector :initial-history (list (dvv:dot +vnode-1+ :counter 1 :timestamp 2))
                                             :initial-dot (dvv:dot +vnode-2+ :counter 1 :timestamp 4)))
         (c2 (dvv:make-dotted-version-vector :initial-history (list (dvv:dot +vnode-1+ :counter 1 :timestamp 3))
                                             :initial-dot (dvv:dot +vnode-3+ :counter 1 :timestamp 5)))
         (c1+c2 (dvv:merge* c1 c2)))
    (assert-true (null (dvv:dotted-version-vector-dot c1+c2)))
    (assert-equal
     `((:actor-id ,+vnode-1+ :counter 1 :timestamp 3)
       (:actor-id ,+vnode-2+ :counter 1 :timestamp 4)
       (:actor-id ,+vnode-3+ :counter 1 :timestamp 5))
     (mapcar #'prelude:to-plist (dvv:dotted-version-vector-history-sorted c1+c2)))))

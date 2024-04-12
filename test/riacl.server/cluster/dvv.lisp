(in-package :riacl.server.tests.cluster)

(defclass test-clock ()
  ((seconds
    :initform 0
    :initarg :seconds)))

(defun advance-clock (clock by-seconds)
  (with-slots (seconds) clock
    (incf seconds by-seconds)))

(defmethod clock:seconds-since-epoch/impl ((clock test-clock))
  (slot-value clock 'seconds))

(defmacro with-test-clock ((clock-name &optional (initial-value 0)) &body body)
  `(let* ((,clock-name (make-instance 'test-clock :seconds ,initial-value))
          (clock:*universal-clock* ,clock-name))
     ,@body))

(serapeum:defconst +vnode-1+ "urn:x-riacl:10:a:1")
(serapeum:defconst +vnode-2+ "urn:x-riacl:10:a:2" )
(serapeum:defconst +vnode-3+ "urn:x-riacl:10:a:3")
(serapeum:defconst +vnode-4+ "urn:x-riacl:10:a:4")
(serapeum:defconst +vnode-5+ "urn:x-riacl:10:a:5")

(define-test dot-equality ()
  "dot= works"
  (let ((a (dvv:dot +vnode-1+ :counter 1 :timestamp 2))
        (b (dvv:dot +vnode-1+ :counter 1 :timestamp 2))
        (c (dvv:dot +vnode-2+ :counter 1 :timestamp 2))
        (d (dvv:dot +vnode-1+ :counter 2 :timestamp 2))
        (e (dvv:dot +vnode-1+ :counter 1 :timestamp 3)))
    (assert-true  (dvv:dot= a a) "Identical object")
    (assert-true  (dvv:dot= a b) "Same values")
    (assert-false (dvv:dot= a c) "Different actors => different dots")
    (assert-false (dvv:dot= a d) "Different counter => different dots")
    (assert-false (dvv:dot= a e) "Different timestamp => different dots")))

(define-test incf-dot-works ()
  (with-test-clock (clock 10)
    (let ((subject (dvv:dot +vnode-1+ :counter 1 :timestamp 5)))
      (dvv:incf-dot subject)
      (assert-equal 2 (dvv:dot-counter subject))
      (assert-equal 10 (dvv:dot-timestamp subject))

      (advance-clock clock 10)
      (dvv:incf-dot subject)

      (assert-equal 3 (dvv:dot-counter subject))
      (assert-equal 20 (dvv:dot-timestamp subject)))))

(define-test descendsp-empty-clock ()
  (let ((empty (dvv:make-dotted-version-vector))
        (c1 (dvv:make-dotted-version-vector
             :initial-history (list (dvv:dot +vnode-1+ :counter 1 :timestamp 1))))
        (c2 (dvv:make-dotted-version-vector
             :initial-dot (dvv:dot +vnode-1+ :counter 2 :timestamp 2))))

    (assert-true (dvv:descendsp c1 empty))
    (assert-true (dvv:descendsp c2 empty))))

(define-test descendsp-self ()
  (let ((c1 (dvv:make-dotted-version-vector
             :initial-history (list (dvv:dot +vnode-1+ :counter 1 :timestamp 1))))
        (c2 (dvv:make-dotted-version-vector
             :initial-dot (dvv:dot +vnode-1+ :counter 2 :timestamp 2))))
    (assert-true (dvv:descendsp c1 c1))
    (assert-true (dvv:descendsp c2 c2))))

(define-test descendsp ()
  (let ((c1 (dvv:make-dotted-version-vector)))
    (dvv:incf-actor c1 +vnode-1+)
    (dvv:incf-actor c1 +vnode-3+)
    (let ((c2 (dvv:copy-dotted-version-vector c1)))
      (dvv:incf-actor c2 +vnode-2+) ; actor vnode-2 only exists in c2
      (dvv:incf-actor c2 +vnode-1+)
      (assert-true (dvv:descendsp c2 c1)))))

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

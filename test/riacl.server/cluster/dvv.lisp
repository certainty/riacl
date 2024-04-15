(in-package :riacl.server.tests)

(define-test dvv-suite)

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

(define-test dot-equality :parent dvv-suite
  (let ((a (dvv:make-dot +vnode-1+ :counter 1 :timestamp 2))
        (b (dvv:make-dot +vnode-1+ :counter 1 :timestamp 2))
        (c (dvv:make-dot +vnode-2+ :counter 1 :timestamp 2))
        (d (dvv:make-dot +vnode-1+ :counter 2 :timestamp 2))
        (e (dvv:make-dot +vnode-1+ :counter 1 :timestamp 3)))
    (true  (dvv:dot= a a) "Identical object")
    (true  (dvv:dot= a b) "Same values")
    (false (dvv:dot= a c) "Different actors => different dots")
    (false (dvv:dot= a d) "Different counter => different dots")
    (false (dvv:dot= a e) "Different timestamp => different dots")))

(define-test incf-dot-works :parent dvv-suite
  (with-test-clock (clock 10)
    (let ((subject (dvv:make-dot +vnode-1+ :counter 1 :timestamp 5)))
      (dvv:incf-dot subject)
      (is = 2 (dvv:dot-counter subject))
      (is = 10 (dvv:dot-timestamp subject))

      (advance-clock clock 10)
      (dvv:incf-dot subject)

      (is = 3 (dvv:dot-counter subject))
      (is = 20 (dvv:dot-timestamp subject)))))

(define-test incf-actor-works :parent dvv-suite
  (with-test-clock (clock 10)
    (let ((subject (dvv:make-dotted-version-vector)))
      ;; empty dvv
      (dvv:incf-actor subject +vnode-1+)
      (true (null (dvv:dotted-version-vector-history subject)))
      (is identifier:identifier= +vnode-1+ (dvv:dot-actor-id (dvv:dotted-version-vector-dot subject)))
      (is = 1 (dvv:dot-counter (dvv:dotted-version-vector-dot subject)))

      (dvv:incf-actor subject +vnode-2+)
      (is = 1 (length (dvv:dotted-version-vector-history subject)))
      (true (dvv:dotted-version-vector-dot subject))

      (is identifier:identifier= +vnode-2+ (dvv:dot-actor-id (dvv:dotted-version-vector-dot subject)))
      (is = 1 (dvv:dot-counter (dvv:dotted-version-vector-dot subject)))


      (dvv:incf-actor subject +vnode-1+)
      (is = 2 (length (dvv:dotted-version-vector-history subject)))
      (is identifier:identifier= +vnode-1+ (dvv:dot-actor-id (dvv:dotted-version-vector-dot subject)))
      (is = 2 (dvv:dot-counter (dvv:dotted-version-vector-dot subject))))))

(define-test descendsp-empty-clock :parent dvv-suite
  (let ((empty (dvv:make-dotted-version-vector))
        (c1 (dvv:make-dotted-version-vector
             :initial-history (list (dvv:make-dot +vnode-1+ :counter 1 :timestamp 1))))
        (c2 (dvv:make-dotted-version-vector
             :initial-dot (dvv:make-dot +vnode-1+ :counter 2 :timestamp 2))))

    (true (dvv:descendsp c1 empty))
    (true (dvv:descendsp c2 empty))))

(define-test descendsp-self
  :parent dvv-suite
  (let ((c1 (dvv:make-dotted-version-vector
             :initial-history (list (dvv:make-dot +vnode-1+ :counter 1 :timestamp 1))))
        (c2 (dvv:make-dotted-version-vector
             :initial-dot (dvv:make-dot +vnode-1+ :counter 2 :timestamp 2))))
    (true (dvv:descendsp c1 c1))
    (true (dvv:descendsp c2 c2))))

(define-test descendsp
  :parent dvv-suite
  (let ((c1 (dvv:make-dotted-version-vector)))
    (dvv:incf-actor c1 +vnode-1+)
    (dvv:incf-actor c1 +vnode-3+)
    (let ((c2 (dvv:copy-dotted-version-vector c1)))
      (dvv:incf-actor c2 +vnode-2+) ; actor vnode-2 only exists in c2
      (dvv:incf-actor c2 +vnode-1+)
      (true (dvv:descendsp c2 c1)))))

(define-test merge-works
  :parent dvv-suite
  (let* ((c1 (dvv:make-dotted-version-vector
              :initial-history (list
                                (dvv:make-dot +vnode-1+ :counter 1 :timestamp 1)
                                (dvv:make-dot +vnode-2+ :counter 2 :timestamp 2))
              :initial-dot (dvv:make-dot +vnode-4+ :counter 4 :timestamp 4)))
         (c2 (dvv:make-dotted-version-vector
              :initial-history (list
                                (dvv:make-dot +vnode-3+ :counter 3 :timestamp 3)
                                (dvv:make-dot +vnode-4+ :counter 3 :timestamp 3)))))

    (true (dvv:emptyp (dvv:merge* (dvv:make-dotted-version-vector))) "Fresh dvv merges to empty dvv")

    (let ((c1-merged-with-c2 (dvv:merge* c1 c2)))
      (true (null (dvv:dotted-version-vector-dot c1-merged-with-c2)))

      (let ((history (mapcar #'prelude:to-plist
                             (dvv:dotted-version-vector-history-sorted c1-merged-with-c2))))
        (is equal
            `((:actor-id ,+vnode-1+ :counter 1 :timestamp 1)
              (:actor-id ,+vnode-2+ :counter 2 :timestamp 2)
              (:actor-id ,+vnode-3+ :counter 3 :timestamp 3)
              ;; event 3 3 is removed for vnode-4
              (:actor-id ,+vnode-4+ :counter 4 :timestamp 4))
            history)))))

(define-test merge-less-left
  :parent dvv-suite
  (let* ((c1 (dvv:make-dotted-version-vector :initial-dot (dvv:make-dot +vnode-1+ :counter 1 :timestamp 1)))
         (c2 (dvv:make-dotted-version-vector :initial-history
                                             (list (dvv:make-dot +vnode-2+ :counter 2 :timestamp 2))
                                             :initial-dot (dvv:make-dot +vnode-3+ :counter 3 :timestamp 3)))
         (c1+c2 (dvv:merge* c1 c2)))
    (true (null (dvv:dotted-version-vector-dot c1+c2)))
    (is equal
        `((:actor-id ,+vnode-1+ :counter 1 :timestamp 1)
          (:actor-id ,+vnode-2+ :counter 2 :timestamp 2)
          (:actor-id ,+vnode-3+ :counter 3 :timestamp 3))
        (mapcar #'prelude:to-plist (dvv:dotted-version-vector-history-sorted c1+c2)))))

(define-test merge-less-right
  :parent dvv-suite
  (let* ((c1 (dvv:make-dotted-version-vector :initial-history (list (dvv:make-dot +vnode-2+ :counter 2 :timestamp 2))
                                             :initial-dot (dvv:make-dot +vnode-3+ :counter 3 :timestamp 3)))
         (c2 (dvv:make-dotted-version-vector :initial-dot (dvv:make-dot +vnode-1+ :counter 1 :timestamp 1)))
         (c1+c2 (dvv:merge* c1 c2)))
    (true (null (dvv:dotted-version-vector-dot c1+c2)))
    (is equal
        `((:actor-id ,+vnode-1+ :counter 1 :timestamp 1)
          (:actor-id ,+vnode-2+ :counter 2 :timestamp 2)
          (:actor-id ,+vnode-3+ :counter 3 :timestamp 3))
        (mapcar #'prelude:to-plist (dvv:dotted-version-vector-history-sorted c1+c2)))))

(define-test merge-same-id
  :parent dvv-suite
  (let* ((c1 (dvv:make-dotted-version-vector :initial-history (list (dvv:make-dot +vnode-1+ :counter 1 :timestamp 2))
                                             :initial-dot (dvv:make-dot +vnode-2+ :counter 1 :timestamp 4)))
         (c2 (dvv:make-dotted-version-vector :initial-history (list (dvv:make-dot +vnode-1+ :counter 1 :timestamp 3))
                                             :initial-dot (dvv:make-dot +vnode-3+ :counter 1 :timestamp 5)))
         (c1+c2 (dvv:merge* c1 c2)))
    (true (null (dvv:dotted-version-vector-dot c1+c2)))
    (is equal
        `((:actor-id ,+vnode-1+ :counter 1 :timestamp 3)
          (:actor-id ,+vnode-2+ :counter 1 :timestamp 4)
          (:actor-id ,+vnode-3+ :counter 1 :timestamp 5))
        (mapcar #'prelude:to-plist (dvv:dotted-version-vector-history-sorted c1+c2)))))

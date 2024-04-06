(in-package :riacl.server.cluster.vector-clock)

;; https://medium.com/geekculture/all-things-clock-time-and-order-in-distributed-systems-logical-clocks-in-real-life-2-ad99aa64753

(deftype counter () '(integer 0 *))

(defclass dot ()
  ((actor
    :reader dot-actor-id
    :initarg :actor-id
    :initform (error "No actor-id provided")
    :type identifier:identifier)
   (counter
    :reader dot-counter
    :initarg :counter
    :initform 1
    :type counter)
   (timestamp
    :reader dot-timestamp
    :initarg :timestamp
    :initform nil
    :type (or null local-time:timestamp))))

(defun make-dot (actor-id &key (counter 1) (timestamp nil timestamp-provided-p))
  (make-instance 'dot :actor-id actor-id :counter counter :timestamp (or (and timestamp-provided-p timestamp) (local-time:now))))

(defmethod print-object ((dot dot) stream)
  (print-unreadable-object (dot stream :type t :identity t)
    (format stream "actor-id: ~A counter: ~A timestamp: ~A" (dot-actor-id dot) (dot-counter dot) (dot-timestamp dot))))

(defun dot-actor= (dot1 dot2)
  "Returns t if dot1 is equal to dot2, nil otherwise."
  (identifier:identifier= (dot-actor-id dot1) (dot-actor-id dot2)))

(defun dot-counter= (dot1 dot2)
  "Returns t if dot1's counter is equal to dot2's counter, nil otherwise."
  (= (dot-counter dot1) (dot-counter dot2)))

(defun dot-counter< (dot1 dot2)
  "Returns t if dot1's counter is less than dot2, nil otherwise."
  (< (dot-counter dot1) (dot-counter dot2)))

(defun dot-counter> (dot1 dot2)
  "Returns t if dot1's counter is greater than dot2, nil otherwise."
  (> (dot-counter dot1) (dot-counter dot2)))

(defun dot-counter<= (dot1 dot2)
  "Returns t if dot1's counter is less than or equal to dot2, nil otherwise."
  (or (dot-counter= dot1 dot2)
      (dot-counter< dot1 dot2)))

(defun dot-counter>= (dot1 dot2)
  "Returns t if dot1's counter is greater than or equal to dot2, nil otherwise."
  (or (dot-counter= dot1 dot2)
      (dot-counter> dot1 dot2)))

(defun dot-timestamp= (dot1 dot2)
  "Returns t if dot1's timestamp is equal to dot2's timestamp, nil otherwise."
  (= (dot-timestamp dot1) (dot-timestamp dot2)))

(defun dot-timestamp< (dot1 dot2)
  "Returns t if dot1's timestamp is less than dot2's timestamp, nil otherwise."
  (< (dot-timestamp dot1) (dot-timestamp dot2)))

(defun dot-timestamp> (dot1 dot2)
  "Returns t if dot1's timestamp is greater than dot2's timestamp, nil otherwise."
  (> (dot-timestamp dot1) (dot-timestamp dot2)))

(defun dot-timestamp<= (dot1 dot2)
  "Returns t if dot1's timestamp is less than or equal to dot2's timestamp, nil otherwise."
  (or (dot-timestamp= dot1 dot2)
      (dot-timestamp< dot1 dot2)))

(defun dot-timestamp>= (dot1 dot2)
  "Returns t if dot1's timestamp is greater than or equal to dot2's timestamp, nil otherwise."
  (or (dot-timestamp= dot1 dot2)
      (dot-timestamp> dot1 dot2)))

(defun dot= (dot1 dot2 &key (ignore-timestamp t))
  "Returns t if dot1 is equal to dot2, nil otherwise."
  (and (dot-actor= dot1 dot2)
       (dot-counter= dot1 dot2)
       (or ignore-timestamp (dot-timestamp= dot1 dot2))))

(defun dot< (dot1 dot2 &key (ignore-timestamp t))
  "Returns t if dot1 is less than dot2, nil otherwise."
  (and (dot-actor= dot1 dot2)
       (dot-counter< dot1 dot2)
       (or ignore-timestamp (dot-timestamp< dot1 dot2))))

(defun dot< (dot1 dot2 &key (ignore-timestamp t))
  "Returns t if dot1 is less than dot2, nil otherwise."
  (and (dot-actor= dot1 dot2)
       (< (dot-counter dot1) (dot-counter dot2))
       (or ignore-timestamp (dot-timestamp< dot1 dot2))))

(defun dot<= (dot1 dot2 &key (ignore-timestamp t))
  "Returns t if dot1 is less than or equal to dot2, nil otherwise."
  (or (dot= dot1 dot2 :ignore-timestamp ignore-timestamp)
      (dot< dot1 dot2 :ignore-timestamp ignore-timestamp)))

(defun dot>= (dot1 dot2 &key (ignore-timestamp t))
  "Returns t if dot1 is greater than or equal to dot2, nil otherwise."
  (or (dot= dot1 dot2 :ignore-timestamp ignore-timestamp)
      (dot> dot1 dot2 :ignore-timestamp ignore-timestamp)))

(defclass vector-clock ()
  ((dots
    :reader vector-clock-dots
    :initarg :dots
    :initform (vector)
    :type (vector dot))))

(defun make-vector-clock (&rest dots)
  (let ((dot-vector (make-array (length dots) :element-type 'dot :adjustable t :fill-pointer t :initial-contents dots)))
    (make-instance 'vector-clock :dots dot-vector)))

(defmethod print-object ((vc vector-clock) stream)
  (print-unreadable-object (vc stream :type t :identity t)
    (format stream "[~{~A~^, ~}]" (coerce (vector-clock-dots vc) 'list))))

(defun vector-clock= (vcA vcB)
  "Returns t if vcA is equal to vcB, nil otherwise."
  (let ((dotsA (vector-clock-dots vcA))
        (dotsB (vector-clock-dots vcB)))
    (every #'dot=
           (sort dotsA #'dot-actor<)
           (sort dotsB #'dot-actor<))))

(-> counter-for-actor-id (vector-clock identifier:identifier) (or null counter))
(defun counter-for-actor-id (vc actor-id)
  "Returns the counter for the given actor-id in the vector clock `vc'."
  (a:when-let ((dot (find-dot-by-actor-id vc actor-id)))
    (dot-counter dot)))

(-> timestamp-for-actor-id (vector-clock identifier:identifier) (or null local-time:timestamp))
(defun timestamp-for-actor-id (vc actor-id)
  "Returns the timestamp for the given actor-id in the vector clock `vc'."
  (a:when-let ((dot (find-dot-by-actor-id vc actor-id)))
    (dot-timestamp dot)))

(-> last-modified (vector-clock) (or null local-time:timestamp))
(defun last-modified (vc)
  "Return the last timestamp from `vc' in a as a `local-time:timestamp'"
  (loop :for dot :accross (vector-clock-dots vc)
        :maximize (or (dot-timestamp dot) 0)))

(-> incf-actor (vector-clock identifier:identifier) vector-clock)
(defun incf-actor (vc actor-id)
  "Increment the counter for the given actor-id in the vector clock `vc'.
If the `actor-id' does not exist in the vector clock, a new `dot' is created with a counter of `1' and `timestamp' of now."
  (a:if-let ((counter (counter-for-actor-id vc actor-id)))
    (setf (dot-counter (find-dot-by-actor-id vc actor-id)) (1+ counter))
    (vector-push-extend (make-dot actor-id) (vector-clock-dots vc))))

(-> emptyp (vector-clock) boolean)
(defun emptyp (vc)
  "Returns t if the vector clock is empty, nil otherwise."
  (zerop (length (vector-clock-dots vc))))

(-> find-dot-by-actor-id (vector-clock identifier:identifier) (or null dot))
(defun find-dot-by-actor-id (vc actor-id)
  "Returns the dot in the vector clock `vc' for the given actor-id, or nil if no such dot exists."
  (find actor-id (vector-clock-dots vc) :key #'dot-actor-id :test #'identifier:identifier=))

(-> descendsp (vector-clock vector-clock) boolean)
(defun descendsp (vcA vcB)
  "Returns `t' if `vcA'  directly descends from `vcB', nil otherwise.
A clock descends from another if all of its dots are greater than or equal to the corresponding dots in the other clock.
All clocks are their own descendants.
Also all clocks descend from the empty clock."
  (let ((dotsA (vector-clock-dots vcA))
        (dotsB (vector-clock-dots vcB)))
    (cond
      ((zerop (length dotsB)) t)
      ((> (lenght dotsB) (length dotsB)) nil)
      (t (every #'(lambda (dotB)
                    (a:when-let ((dotA (find-dot-by-actor-id- vcA (dot-actor-id dotB))))
                      (dot<= dotA dotB)))
                dotsB)))))

(-> descends-dot-p (vector-clock dot) boolean)
(defun descends-dot-p (vc dot)
  "Returns `t' if the vector clock `vc' descends from the given `dot', `nil' otherwise."
  (descendsp vc (make-vector-clock dot)))

(-> dominatesp (vector-clock vector-clock) boolean)
(defun dominatesp (vcA vcB)
  "Returns t if vc1 dominates vc2, nil otherwise. "
  (and (descendsp vcA vcB)
       (not (descendsp vcB vcA))))

(-> ==> (vector-clock vector-clock) boolean)
(defun ==> (vcA vcB)
  "Returns t if vc1 causes vc2, nil otherwise."
  (dominatesp vcB vcA))

(-> concurrentp (vector-clock vector-clock) boolean)
(defun concurrentp (vcA vcB)
  "Returns t if vc1 is concurrent with vc2, nil otherwise."
  (not (or (==> vcA vcB)
           (==> vcB vcA))))

(defun merge (&rest vcs)
  "Combine all vector clocks `vc' into their least possible common descendant."
  )

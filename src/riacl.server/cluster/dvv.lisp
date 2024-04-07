(in-package :riacl.server.cluster.dvv)

;;; This is an implementation of dotted version vectors in Common Lisp.
;;; It's based on the erlang implementation here: https://github.com/ricardobcl/Dotted-Version-Vectors/blob/27fd1225c10a7dc4890e5e55abddf6db2392f6cc/src/dvv.erl
;;; Please refer to the class and method documentation for details.
;;;
;;; References:
;;;  * http://gsd.di.uminho.pt/members/vff/dotted-version-vectors-2012.pdf
;;;  * https://medium.com/geekculture/all-things-clock-time-and-order-in-distributed-systems-logical-clocks-in-real-life-2-ad99aa64753

(deftype counter ()
  "A logical clock denoting the order of events."
  '(integer 0 *))

(deftype timestamp ()
  "Seconds since epoch 00:00 of January 1, 1900 in the GMT time zone.
Same as returned by `get-universal-time'."
  '(integer 0  *))

(defclass dot ()
  ((actor-id
    :reader dot-actor-id
    :initarg :actor-id
    :initform (error "No actor-id provided")
    :type identifier:identifier
    :documentation "The actor-id of the actor that has knowledge of this event. This is usually the vnode id.")
   (counter
    :reader dot-counter
    :initarg :counter
    :initform 1
    :type version
    :documentation "The logical clock value of the event. It is strictly monotonically increasing.")
   (timestamp
    :reader dot-timestamp
    :initarg :timestamp
    :initform nil
    :type (or null timestamp)
    :documentation "The wall clock time when the event happened. Which is useful for debugging, logging and representation to the client."))
  (:documentation
   "A dot represents a single discrete event that happened in the system.
If you think about a traditional vector clock of let's say (A, 4), then this states that the sytem knows of 4 events by A.
It's a compact form of the events that happened. If were to fill in the dots, that led to this vector clock, it would look like this: (A, 1), (A, 2), (A, 3), (A, 4).
Each of those steps is a dot."))

(-> dot (identifier:identifier &key (:counter counter) (:timestamp timestamp)) (values dot &optional))
(defun dot (actor-id &key (counter 1) (timestamp nil timestamp-provided-p))
  (make-instance 'dot :actor-id actor-id :counter counter :timestamp (or (and timestamp-provided-p timestamp) (get-universal-time))))

(defmethod print-object ((dot dot) stream)
  (print-unreadable-object (dot stream :type t :identity t)
    (format stream "actor-id: ~A counter: ~A timestamp: ~A" (dot-actor-id dot) (dot-counter dot) (dot-timestamp dot))))

(defmethod prelude:to-plist ((dot dot))
  (with-slots (actor-id counter timestamp) dot
    `(:actor-id ,actor-id :counter ,counter :timestamp ,timestamp)))

(-> dot= (dot dot) boolean)
(defun dot-actor= (dot1 dot2)
  "Returns `t' if the actor-id of `dot1' is equal to the actor-id of `dot2', `nil' otherwise."
  (identifier:identifier= (dot-actor-id dot1) (dot-actor-id dot2)))

(-> dot-counter= (dot dot) boolean)
(defun dot-counter= (dot1 dot2)
  "Returns `t' if the counter of `dot1' is equal to the counter of `dot2', `nil' otherwise."
  (= (dot-counter dot1) (dot-counter dot2)))

(-> dot< (dot dot) boolean)
(defun dot-counter< (dot1 dot2)
  "Returns `t' if the counter of `dot1' is less than the counter of `dot2', `nil' otherwise.'"
  (< (dot-counter dot1) (dot-counter dot2)))

(-> dot> (dot dot) boolean)
(defun dot-counter> (dot1 dot2)
  "Returns `t' if the counter of `dot1' is greater than the counter of `dot2', `nil' otherwise."
  (> (dot-counter dot1) (dot-counter dot2)))

(-> dot-counter<= (dot dot) boolean)
(defun dot-counter<= (dot1 dot2)
  "Returns `t' if the counter of `dot1' is less than or equal to the counter of `dot2', `nil' otherwise."
  (s:true (or (dot-counter= dot1 dot2)
              (dot-counter< dot1 dot2))))

(-> dot-counter>= (dot dot) boolean)
(defun dot-counter>= (dot1 dot2)
  "Returns `t' if the counter of `dot1' is greater than or equal to the counter of `dot2', `nil' otherwise."
  (s:true (or (dot-counter= dot1 dot2)
              (dot-counter> dot1 dot2))))

(defclass dotted-version-vector ()
  ((history
    :reader dotted-version-vector-history
    :initarg :history
    :initform nil
    :type list
    :documentation "The causal history as recorded in the version vector.")
   (dot
    :reader dotted-version-vector-dot
    :initarg :dot
    :initform nil
    :type (or null dot)
    :documentation "The most recent dot in the version vector."))
  (:documentation
   "A dotted version vector is used to track the causality of events in a distributed system.
It is an accurate record of the events that have happened in the system, and the order in which they happened.
It is an improvement to a simple vector clock, as it allows to use vnode-ids as actors, while also addressing
the problems that arise from that.

vnode-ids are used, rather than client-ids, to avoid an explosion of values in the version vector.
However since vnodes are not the actual unit of concurrency, the client-id is, this can lead to problems with concurrent writes.
A typical operation for that case is to create a merged version of the value, creating a sibling.
The amounts of siblings grow more than necessary because version vectors can't accurately know if a certain actor have knowledge about a certain event.

Let's consider the following example:

C1 and C2 are two clients, A is the vnode handling the request, and v1 to vn are values.
Furthermore, all request happen for the same key.

C1 ---  [ v1, () ]    ---> A
C1 <--- [ v1, (A,1) ] ---- A

C1 stores the value v1 providing an empty context, meaning the client has no causal history of this value.
This is recorded by the system and since the value didn't exist before, it's stored as {v1} ~ (A,1).

C2 --- [ v2, () ] ------> A

Now C2 issues a write with value v2, again with empty context, meaning this client has no causal history of this value.
A typical operation for A is to merge create a sibling and merge the version vectors.
So the state on A becomes {v1, v2} ~ (A,2). This means we lose the information that v1 was associated with (A,1).

C1 --- [v3, (A,1)] ----> A

Now C1 issues an update again, not knowing about the write that was done by C2 in the meantime.
It provides its knowledge about the event, which is (A,1).
Again A has no other option than to create a sibling, and the state becomes {v1, v2, v3} ~ (A,3).


## Dotted version vectors

The dotted version vector stores the combined versions but also the most recent dot, which gives additional context, that allows to resolve the above mentioned scenario.
It can accurately detect if a certain actor has knowledge about a certain event. Let's see an example:

C1 and C2 are two clients, A is the vnode handling the request, and v1 to vn are values.
Furthermore, all request happen for the same key.

C1 ---  [ v1, () ]    ---> A
C1 <--- [ v1, (A,1) ] ---- A

C1 stores the value v1 providing an empty context, meaning the client has no causal history of this value.
This is recorded by the system and since the value didn't exist before, it's stored as {v1} ~ ([], (A,1)).
This simply means, the value has an enpty version vector and the most recent dot is (A,1).

C2 --- [ v2, () ] ------> A

Now C2 issues a write with value v2, again with empty context, meaning this client has no causal history of this value.
The system stores this value as (v2, [], {A,2}) which coexists with the already existing value {v1} ~ ([], (A,1)).

C1 --- [v3, (A,1)] ----> A

Now C1 issues an update, not knowing about the write that was done by C2 in the meantime.
In provides the causal history, that it is aware of as context (A,1).

Now the system can do the following:
1. it can discard {v1} ~ ([], (A,1)) because given the history, we know that C1 has knowledte about this event.
2. it keeps {v2} ~ ([], (A,2)) because C1 obviously doesn't know about it
3. it stores {v3} ~ ( [(A,1)], (A,3) ) to track the history of A in the version vector and record the most recent event.

The final state on A is:

v2 ~ ([], (A,2))
v3 ~ ([(A,1)], (A,3))


Ref: http://gsd.di.uminho.pt/members/vff/dotted-version-vectors-2012.pdf
"))

(-> make-dotted-version-vector (&key (:initial-history list) (:initial-dot (or null dot))) (values dotted-version-vector &optional))
(defun make-dotted-version-vector (&key (initial-history nil) (initial-dot nil))
  "Create an empty dotted version vector."
  (make-instance 'dotted-version-vector :history initial-history :dot initial-dot))

(defmethod print-object ((dvv dotted-version-vector) stream)
  (print-unreadable-object (dvv stream :type t :identity t)
    (format stream "([~{~A~^, ~}], ~A)"  (dotted-version-vector-history dvv) (dotted-version-vector-dot dvv))))

(defmethod prelude:to-plist ((dvv dotted-version-vector))
  (with-slots (history dot) dvv
    `(:history ,(mapcar #'prelude:to-plist history) :dot ,dot)))

(-> emptyp (dotted-version-vector) boolean)
(defun emptyp (dvv)
  (with-slots (history dot) dvv
    (s:true (and (null history) (not dot)))))

(defun dotted-version-vector-history-sorted (dvv &key (merge-dot nil))
  "Returns the history of `dvv' sorted by `actor-id'.
When `merge-dot' is `t' and `dvv' has a dot, its added to the history before sorting.
  "
  (with-slots (history dot) dvv
    (sort
     (or (and merge-dot (merge-dot-to-history dot history)) history)
     #'identifier:identifier< :key #'dot-actor-id)))

(defun merge* (&rest dvvs)
  (cond
    ((null dvvs) (make-dotted-version-vector))
    ((null (cdr dvvs)) (with-slots (history dot) (car dvvs)
                         (make-dotted-version-vector :initial-history (merge-dot-to-history dot history))))
    (t (reduce #'merge2 dvvs))))

(defun merge2 (dvvA dvvB)
  (make-dotted-version-vector
   :initial-history
   (merge-histories
    (dotted-version-vector-history-sorted dvvA :merge-dot t)
    (dotted-version-vector-history-sorted dvvB :merge-dot t))))

(defun merge-histories (historyA historyB)
  (let ((newHistory nil))
    (loop
      (let ((dotA (car historyA))
            (dotB (car historyB)))
        (cond
          ((null dotA)
           (return-from merge-histories (append (nreverse newHistory) historyB)))
          ((null dotB)
           (return-from merge-histories (append (nreverse newHistory) historyA)))
          ((identifier:identifier< (dot-actor-id dotA) (dot-actor-id dotB))
           (push dotA newHistory)
           (setf historyA (rest historyA)))
          ((identifier:identifier> (dot-actor-id dotA) (dot-actor-id dotB))
           (push dotB newHistory)
           (setf historyB (rest historyB)))
          (t
           (push (merge-dots dotA dotB) newHistory)
           (setf historyA (rest historyA)
                 historyB (rest historyB))))))))

(defun merge-dots (a b)
  "Merges two dots `a' and `b' and returns a new dot as a result of it.
   When the counters are equal it discards `b' merges the timestamp using the maximum of both.
  "
  (cond
    ((dot-counter> a b) a)
    ((dot-counter< a b) b)
    (t (dot (dot-actor-id a)
            :counter (dot-counter a)
            :timestamp (max (dot-timestamp a) (dot-timestamp b))))))


(defun merge-dot-to-history (dot history)
  "Merges the `dot' into the `history' and creates a new `history'.
This overwrites any existing `dot' with the same `actor-id'."
  (if (null dot)
      history
      (let ((new-history (remove-if (lambda (item) (dot-actor= dot item)) history)))
        (cons dot new-history))))

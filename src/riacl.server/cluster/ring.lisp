(in-package :riacl.server.cluster.ring)

(s:defconst +hash-size+ 160)
(s:defconst +ring-top+ (expt 2 +hash-size+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun power-of-two-p (n)
    "Checks if `n' is a power of two"
    (and (plusp n)
         (= 1 (denominator (rationalize (log n 2)))))))

(deftype ring-size () '(and (integer 2 *) (satisfies power-of-two-p)))

(defclass consistent-hash-ring ()
  ((number-of-partitions
    :reader number-of-partitions
    :initarg :number-of-partitions
    :initform (error "number-of-partitions must be provided")
    :type ring-size
    :documentation "The number of partitions in the ring. Must be a power of 2.")
   (partitions
    :reader ring-partitions
    :initarg :partitions
    :initform (error "partitions must be provided")
    :type vector)))


(-> make-ring (ring-size (integer)))
(defun make-ring (size seed-node)
  (make-instance 'consistent-hash-ring
                 :size size
                 :partitions (make-partitions size seed-node)))

(defmethod print-object ((ring consistent-hash-ring) stream)
  (print-unreadable-object (ring stream :type t)
    (format stream "size: ~A, partitions: ~a"
            (ring-size ring)
            (ring-partitions ring))))

(defun power-of-two-p (n)
  "Checks if `n' is a power of two"
  (and (plusp n)
       (= 1 (denominator (rationalize (log n 2))))))

(defclass partition ()
  ((start
    :reader partition-start
    :initarg :start
    :initform (error "start must be provided")
    :type (integer 0 *))
   (claimed-by
    :reader partition-claimed-by
    :initarg :claimed-by
    :initform nil
    :type (or null virtual-node)))
  (:documentation "A partition denotes a segment of the consistent hash-ring which is claimed by a vnode. The interval that is covered by each partition is determined by the ring-size (the number of partitons)"))

(defmethod print-object ((partition partition) stream)
  (print-unreadable-object (partition stream :type t)
    ;; fomrat start and end as power of 2
    (format stream "{~A, ~A}"
            (partition-start partition)
            (partition-claimed-by partition))))

(defun make-partition (start seed-node)
  (make-instance 'partition :start start :claimed-by seed-node))

(-> make-partitions (ring-size t) (vector partition *))
(defun make-partitions (num seed-node)
  "Create and return a vector containing `num' partitions'.
All partitions are initially claimed by the provided `seed-node'."
  (let ((increment (ring-increment num))
        (partitions (make-array num)))
    (loop for idx from 0
          for i from 0 below +ring-top+ by increment
          do (setf (aref partitions idx)
                   (make-partition (if (zerop i) 0 (1+ i)) seed-node))
          finally (return partitions))))

(defun ring-increment (num-partitions)
  (truncate +ring-top+ num-partitions))

(-> ring-hash ((vector (unsigned-byte 8)))  (unsigned-byte 160))
(defun ring-hash (key)
  "Hash a key to a 160-bit integer."
  (prog1
      (ironclad:octets-to-integer
       (ironclad:digest-sequence :sha1 key))))

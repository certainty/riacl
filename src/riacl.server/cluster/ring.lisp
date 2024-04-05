(in-package :riacl.server.cluster.ring)

;;; Implementation of a consistent hash ring

(s:defconst +hash-size+ 160)
(s:defconst +ring-top+ (expt 2 +hash-size+))

(defclass consistent-hash-ring ()
  ((size
    :reader ring-size
    :initarg :size
    :initform (error "size must be provided")
    :type (integer 0 *)
    :documentation "The number of partitions in the ring. Must be a power of 2.")
   (node-count
    :reader ring-node-count
    :initarg :node-count
    :initform 0
    :type (integer 0 *))
   (partitions
    :reader ring-partitions
    :initarg :partitions
    :initform (error "partitions must be provided")
    :type vector)))

(defun make-ring (size node-count seed-node)
  (let ((partitions (make-partitions size seed-node)))
    (make-instance 'consistent-hash-ring :size size
                                         :node-count node-count
                                         :partitions partitions)))

(defmethod print-object ((ring consistent-hash-ring) stream)
  (print-unreadable-object (ring stream :type t)
    (format stream "size: ~A, node-count: ~A , partitions: ~a"
            (ring-size ring)
            (ring-node-count ring)
            (ring-partitions ring))))

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
    :type (or null virtual-node))))

(defmethod print-object ((partition partition) stream)
  (print-unreadable-object (partition stream :type t)
    ;; fomrat start and end as power of 2
    (format stream "{~A, ~A}" (partition-start partition)
            (partition-end partition)
            (partition-claimed-by partition))))

(defun make-partition (start seed-node)
  (make-instance 'partition :start start :claimed-by seed-node))

(defun make-partitions (size seed-node)
  "Create a vector of partitions for our ring. All partitions are initially unclaimed."
  (let ((increment (ring-increment size))
        (partitions (make-array size)))
    (loop for idx from 0
          for i from 0 below +ring-top+ by increment
          do (setf (aref partitions idx)
                   (make-partition (if (zerop i) 0 (1+ i)) seed-node))
          finally (return partitions))))

(defun ring-increment (ring-size)
  (truncate +ring-top+ ring-size))

(-> ring-hash ((vector (unsigned-byte 8)))  (unsigned-byte 160))
(defun ring-hash (key)
  "Hash a key to a 160-bit integer."
  (prog1
      (ironclad:octets-to-integer
       (ironclad:digest-sequence :sha1 key))))

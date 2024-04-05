(in-package :riacl.server.cluster)

(s:defconst +hash-size+ 160)
(s:defconst +ring-top+ (expt 2 +hash-size+))

;; TODO: make generic method for hashing
(-> hash-function ((vector (unsigned-byte 8)))  (unsigned-byte 160))
(defun hash-function (key)
  "Hash a key to a 160-bit integer."
  (prog1
      (ironclad:octets-to-integer
       (ironclad:digest-sequence :sha1 key))))

(defclass ring ()
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
    (make-instance 'ring :size size
                         :node-count node-count
                         :partitions partitions)))

(defmethod print-object ((ring ring) stream)
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
   (end
    :reader partition-end
    :initarg :end
    :initform (error "end must be provided")
    :type (integer 0 *))
   (claimed-by
    :reader partition-claimed-by
    :initarg :claimed-by
    :initform nil
    :type (or null virtual-node))))

(defmethod print-object ((partition partition) stream)
  (print-unreadable-object (partition stream :type t)
    ;; fomrat start and end as power of 2
    (format stream "[~A, ~A] => ~A" (partition-start partition)
            (partition-end partition)
            (partition-claimed-by partition))))

(defun make-partition (start end seed-node)
  (make-instance 'partition :start start :end end :claimed-by seed-node))

(defun make-partitions (size seed-node)
  "Create a vector of partitions for our ring. All partitions are initially unclaimed."
  (let ((increment (ring-increment size))
        (partitions (make-array size)))
    (loop for idx from 0
          for i from 0 below +ring-top+ by increment
          do (setf (aref partitions idx)
                   (make-partition (if (zerop i) 0 (1+ i)) (+ i increment) seed-node))
          finally (return partitions))))

(defun ring-increment (ring-size)
  (truncate +ring-top+ ring-size))

(defun claim-partition (ring idx node)
  "Claim the partition at index `idx' for `node'"
  (let ((partitions (ring-partitions ring)))
    (setf (partition-claimed-by (aref partitions idx)) node)
    ring))

(defun chash (object-name)
  "Given any term used to name an object, produce that object's key into the ring."
  (let ((key (convert-to-key object-name)))
    ;; Assuming you have a function to hash keys (similar to SHA-1)
    (hash-function key)))

(defun update-partition (index-as-int name ring)
  "Make the partition beginning at IndexAsInt owned by Name'd node."
  (let* ((partitions (ring-partitions ring))
         (partition (aref partitions index-as-int)))
    (setf (partition-claimed-by partition) name)
    ring))

(defun lookup-partition (index-as-int ring)
  "Find the Node that owns the partition identified by IndexAsInt."
  (let* ((partitions (ring-partitions ring))
         (partition (aref partitions index-as-int)))
    (partition-claimed-by partition)))

(defun next-index (integer-key ring)
  "Given the integer representation of a chash key, return the next ring index integer value."
  (let* ((num-partitions (ring-size ring))
         (inc (ring-increment num-partitions)))
    (* (mod (+ (truncate integer-key inc) 1) num-partitions) inc)))

(defun merge-rings (ring-a ring-b)
  "Return a randomized merge of two rings."
  (let* ((size (ring-size ring-a))
         (partitions-a (ring-partitions ring-a))
         (partitions-b (ring-partitions ring-b))
         (merged-partitions (map 'vector (lambda (partition-a partition-b)
                                           (make-partition (partition-start partition-a)
                                                           (partition-end partition-a)
                                                           (random-node (partition-claimed-by partition-a)
                                                                        (partition-claimed-by partition-b))))
                                 partitions-a partitions-b)))
    (make-instance 'ring :size size
                         :node-count (+ (ring-node-count ring-a) (ring-node-count ring-b))
                         :partitions merged-partitions)))

(defun random-node (node-a node-b)
  "Return a random node between two nodes."
  (if (equal node-a node-b)
      node-a
      (nth (random 2) (list node-a node-b))))

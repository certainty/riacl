(in-package :riacl.server.cluster)

(s:defconst +state-version+ 1)
(s:defconst +membership-state+ (list :joining :valid :invalid :leaving :exiting :down))

(defun membership-state-p (state)
  (member state +membership-state+))

(deftype membership-state () '(satisfies membership-state-p))

(defclass cluster-member ()
  ((node
    :reader member-node
    :initarg :node
    :initform (error "node is required")
    :type identifier:identifier)
   (vector-clock
    :reader member-vector-clock
    :initarg :vector-clock
    :initform (error "vector-clock is required")
    :type dvv:dotted-version-vector)
   (state
    :reader member-state
    :initarg :state
    :initform :invalid
    :type membership-state)))

(defclass state ()
  ((version
    :reader state-version
    :initarg :version
    :initform +state-version+
    :type (integer 1 *))
   (node-id
    :reader state-node-id
    :initarg :node-id
    :initform (error "node-id is required")
    :type identifier:identifier)
   (cluster-name
    :reader state-cluster-name
    :initarg :cluster
    :initform (error "cluster-name is required")
    :type identifier:identifier)
   (vector-clock
    :reader state-vector-clock
    :initarg :vector-clock
    :initform (error "vector-clock is required")
    :type dvv:dotted-version-vector)
   (members
    :reader state-members
    :initarg :members
    :initform (error "members is required")
    :type hash-table)
   (ring
    :reader state-ring
    :initarg :ring
    :initform (error "ring is required")
    :type ring:consistent-hash-ring))
  (:documentation "The state that represents the knowledge of a particular node about the cluster. This state is passed around in the cluster and is the core of the distributed state machine."))

(defun make-state (cluster-name node-id)
  "Create a fresh state for the (physical) node with `node-id' and the given `cluser-name'"
  (make-instance 'state
                 :cluster-name cluster-name
                 :node-id node-id
                 :vector-clock (dvv:make-dotted-version-vector)
                 :members (make-hash-table :test #'identifier:identifier=)
                 :ring (ring:make-ring)))

(defun serialize-state ()
  "Serialize the state to be send over the wire"
  t)

(defun deserialize-state (&key (version +state-version+))
  "Deserialize the state from the wire format"
  t)

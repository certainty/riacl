(in-package :riacl.server.cluster)

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
    :type vector-clock:vector-clock)
   (state
    :reader member-state
    :initarg :state
    :initform :invalid
    :type membership-state))
  (:documentation "A member of the cluster. This is the core of the membership protocol."))

(defclass state ()
  ((version
    :reader state-version
    :initarg :version
    :initform 1
    :type (integer 1 *))
   (node
    :reader state-name
    :initarg :node-name
    :initform (error "node-name is required")
    :type identifier:identifier)
   (cluster
    :reader state-cluster
    :initarg :cluster
    :initform (error "cluster-name is required")
    :type identifier:identifier)
   (vector-clock
    :reader state-vector-clock
    :initarg :vector-clock
    :initform (error "vector-clock is required")
    :type vector-clock:vector-clock)
   (members
    :reader state-members
    :initarg :members
    :initform (error "members is required")
    :type list)
   (ring
    :reader state-ring
    :initarg :ring
    :initform (error "ring is required")
    :type ring:consistent-hash-ring))
  (:documentation "The state that represents the knowledge of a particular node about the cluster. This state is passed around in the cluster and is the core of the distributed state machine."))

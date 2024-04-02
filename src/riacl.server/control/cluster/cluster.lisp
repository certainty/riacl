(in-package :riacl.server.cluster)

(defclass cluster-node ()
  ((id
    :reader cluster-node-id
    :initarg :id
    :initform (error "id is required"))
   (address
    :reader cluster-node-address
    :initarg :address
    :initform (error "address is required")
    :type network:network-address)))

(defclass seed-node (cluster-node)
  ((neighbours
    :reader seed-node-neighbours
    :initarg :neighbours
    :initform (make-hash-table :test 'equal))))

(defun make-cluster-node (id address)
  (make-instance 'cluster-node :id id :address address))

(defclass cluster-state ()
  ((known-nodes
    :reader cluster-known-nodes
    :initarg :known-nodes
    :initform (make-hash-table :test 'equal))
   (seed-nodes
    :reader cluster-seed-nodes
    :initarg :seed-nodes
    :initform (make-hash-table :test 'equal))
   (local-node
    :reader cluster-local-node
    :initarg :local-node
    :initform nil
    :type (or null cluster-node))))

(defun make-cluster-state ()
  (make-instance 'cluster-state))

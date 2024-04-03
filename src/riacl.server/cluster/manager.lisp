(in-package :riacl.server.cluster)

(defclass cluster-manager ()
  ((cluster-name
    :reader cluster-name
    :initarg :cluster-name
    :initform (error "cluster-name is required")
    :type string)
   (state
    :initarg :state
    :initform (error "state is required")
    :type cluster-state)))

(defun manager-for (cluster-name)
  "Creates a new cluster manager for the given `cluster-name'."
  (let ((state (make-cluster-state)))
    (make-instance 'cluster-manager :cluster-name cluster-name :state state)))

(defun local-node (manager)
  "Returns the local node for the cluster managed by `manager'."
  (with-slots (state) manager
    (with-slots (local-node) state
      local-node)))

(defun node-id (cluster-name listen-address)
  "Generates a cluster node id, which is URN consisting of the cluster name, the listen address of the node and a random 64bit ID"
  (let ((id (random (expt 2 64)))
        (ip-address (network:network-address-host listen-address)))
    (format nil "urn:x-riacl:~A:~A:~X" cluster-name ip-address id)))

(defun join-this-node (manager)
  "Joins the current node to the cluster managed by `manager'. Returns the new node."
  (with-slots (state cluster-name) manager
    (with-slots (local-node) state
      (when local-node
        (error "This node has already joined the cluster"))
      (setf local-node (make-cluster-node (node-id cluster-name config:*control.listen-address*) config:*control.listen-address*))
      local-node)))

(defun leave-this-node (manager)
  "Leaves the current node from the cluster managed by `manager'."
  (with-slots (state) manager
    (with-slots (local-node) state
      (when (null local-node)
        (error "This node has not joined the cluster"))
      (setf local-node nil))))

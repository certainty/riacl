(in-package :riacl.server.cluster)

(defclass cluster-manager ()
  ((state
    :initarg :state
    :initform (error "state is required")
    :type state)))

(defun make-manager ()
  (let ((cluster-name (config:cluster.name))
        (node-id (config:cluster.node-id)))
    (make-instance 'cluster-manager :state (make-state cluster-name node-id))))

(defun node-id (manager)
  "Returns the id of the physical node where the manager is started. The node might not be part of the cluster yet."
  (with-slots (state) manager
    (state-node-id state)))

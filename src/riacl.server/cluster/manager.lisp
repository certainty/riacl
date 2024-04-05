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


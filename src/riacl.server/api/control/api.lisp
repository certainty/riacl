(in-package :riacl.server.api.control)

(defclass control-api (api:api-endpoint)
  ((cluster-node-id
    :reader data-api-cluster-node-id
    :initarg :cluster-node-id
    :initform (error "No cluster node ID provided")))
  (:documentation "The control plane API server"))

(defun register-routes (endpoint)
  (api:add-route endpoint #'handle-ping "/ping" :method :get))

(defun handle-ping (endpoint params)
  (declare (ignore endpoint params))
  (api:respond-with `((:message . "pong"))))

(defun make-control-api (cluster-node-id)
  (let ((endpoint (make-instance 'control-api
                                 :cluster-node-id cluster-node-id
                                 :listen-address config:*api.control.listen-address*
                                 :name "control-api")))
    (register-routes endpoint)
    endpoint))

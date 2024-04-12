(in-package :riacl.server.api.data)

(defclass data-api (api:api-endpoint)
  ((cluster-node-id
    :reader data-api-cluster-node-id
    :initarg :cluster-node-id
    :initform (error "No cluster node ID provided")))
  (:documentation "The data plane API server"))

(defun register-routes (endpoint)
  (api:add-routes endpoint
                  ((:get "/ping" #'handle-ping))))


(defun handle-ping (endpoint params)
  (declare (ignore endpoint params))
  (api:respond-with `((:message . "pong"))))

(defun make-data-api (cluster-node-id)
  (let ((endpoint (make-instance 'data-api
                                 :cluster-node-id cluster-node-id
                                 :listen-address config:*api.data.listen-address*
                                 :name "data-api")))
    (register-routes endpoint)
    endpoint))

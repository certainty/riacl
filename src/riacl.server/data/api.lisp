(in-package #:riacl.server.data.api)

(defclass api-app (ningle:app) ())

(defclass data-api ()
  ((cluster-node-id
    :reader data-api-cluster-node-id
    :initarg :cluster-node-id
    :initform (error "No cluster node ID provided"))
   (listen-address
    :reader data-api-listen-address
    :initarg :listen-address
    :initform (error "No listen address provided"))
   (handler
    :reader data-api-handler
    :initarg :handler
    :initform (error "No handler provided"))
   (app
    :reader data-api-app
    :initarg :app
    :initform (error "No app provided")))
  (:documentation "The data plane API server"))

(defun register-routes (api)
  (with-slots (app) api
    (setf (ningle:route app "/ping" :method :get) (handle-ping api))))

(defun handle-ping (api)
  (lambda (params)
    (declare (ignore params))
    (respond-with api `((:message . "pong")))))

(defun start-api (cluster-node-id)
  (let* ((address (network:network-address-host config:*api.listen-address*))
         (port (network:network-address-port config:*api.listen-address*))
         (app (make-instance 'api-app))
         (handler (clack:clackup app :address address :port port :server :hunchentoot))
         (api (make-instance 'data-api
                             :cluster-node-id cluster-node-id
                             :listen-address config:*api.listen-address*
                             :handler handler
                             :app app)))
    (log:info "[Data] API started on ~A" config:*api.listen-address*)
    (register-routes api)
    api))

(defun stop-api (data-api)
  (with-slots (handler) data-api
    (clack:stop handler)
    (log:info "[Data] API stopped")))

;; support functions
(defun request-header (name &optional default)
  (or (cdr (assoc name (lack.request:request-headers ningle:*request*))) default))

(defun encoder-for (content-type)
  (cond
    ((string-equal content-type "application/json")
     (lambda (data)
       (with-output-to-string (s)
         (let ((json:*json-output* s))
           (json:encode-json data)))))
    (t #'identity)))

(defun respond-with (api data &key (status 200) (headers '()) (content-type "application/json"))
  (let ((full-headers `(:content-type ,content-type ,@headers ,@(standard-headers api))))
    (setf (lack.response:response-headers ningle:*response*)
          (append (lack.response:response-headers ningle:*response*) full-headers))
    (setf (lack.response:response-status ningle:*response*) status)
    (funcall (encoder-for content-type) data)))

(defun standard-headers (api)
  (with-slots (cluster-node-id) api
    `(:riacl-cluster-node-id ,cluster-node-id)))

(defun response-error (api status id &rest details)
  (respond-with api `((:id . ,id) (:details ,@details))
                :status status))

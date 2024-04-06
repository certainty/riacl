(in-package :riacl.server.api.foundation)


(defclass api-app (ningle:app) ())

(defclass api-endpoint ()
  ((name
    :reader api-name
    :initarg :name
    :initform (error "No name specified"))
   (listen-address
    :reader api-listen-address
    :initarg :listen-address
    :initform (error "No listen address specified"))
   (handler
    :reader api-handler
    :initarg :handler
    :initform nil)
   (app
    :reader api-app
    :initarg :app
    :initform (make-instance 'api-app))))

(defmethod print-object ((endpoint api-endpoint) stream)
  (print-unreadable-object (endpoint stream :type t :identity nil)
    (format stream "name: ~A listen: ~A" (api-name endpoint) (api-listen-address endpoint))))

(defun make-api-endpoint (name listen-address)
  (make-instance 'api-endpoint :name name :listen-address listen-address))

(defmacro add-route (api-endpoint handler &rest ningle-route-args)
  "Add a route to the given API endpoint. Handler needs to be a function that accepts two arguments `(api-endpoint params)' "
  `(setf (ningle:route (api-app ,api-endpoint) ,@ningle-route-args)
         (lambda (params)
           (funcall ,handler ,api-endpoint params))))

(defun get-request-header (name &optional default)
  "Get the value of the request header with the given name. If the header is not found, return the default value."
  (or (cdr (assoc name (lack.request:request-headers ningle:*request*))) default))

(defgeneric expand-content-type (key)
  (:documentation "Expand the given content-type key to a full content-type string."))

(defmethod expand-content-type ((key (eql :json)))
  "Expand the given content-type key to a full content-type string."
  "application/json")

(defmethod expand-content-type ((key (eql :msgpack)))
  "Expand the given content-type key to a full content-type string."
  "application/msgpack")

(defmethod expand-content-type ((key string))
  "Expand the given content-type key to a full content-type string."
  key)

(defparameter *content-encoders* (make-hash-table :test 'equal) "The list of registered content encoders")

(defun register-encoder (content-type encoder)
  "Register a new encoder for the given content-type. The encoder should be a function that takes the content and returns the encoded content."
  (setf (gethash (expand-content-type content-type) *content-encoders*) encoder))

(defun encode-content (content-type content)
  "Encode the content according to the content-type. If no encoder is found, the content is returned as is."
  (let ((encoder (gethash (expand-content-type content-type) *content-encoders* #'identity)))
    (funcall encoder content)))

(defmacro define-encoder (content-type (data) &body body)
  "Define a new encoder with the given name."
  `(register-encoder ,content-type (lambda (,data) ,@body)))

(define-encoder :json (data)
  (with-output-to-string (s)
    (let ((json:*json-output* s))
      (json:encode-json data))))

(define-encoder :msgpack (data)
  (messagepack:encode data))

(defun respond-with (data &key (status 200) (headers '()) (content-type :json))
  "Create an API response with the given data. Make sure to only call this function once per request.
   The content will be encoded according to the content-type. See also `register-encoder' to add new encoders.
  "
  (let ((full-headers `(:content-type ,(expand-content-type content-type) ,@headers)))
    (setf (lack.response:response-headers ningle:*response*)
          (append (lack.response:response-headers ningle:*response*) full-headers))
    (setf (lack.response:response-status ningle:*response*) status)
    (encode-content content-type data)))

(defun respond-with-error (status id &rest details)
  "Respond with an error providing the status code, id, and details."
  (respond-with `((:error (:id ,id :details ,details))) :status status))

(defun start-api (api-endpoint &key (server :hunchentoot))
  "Starts the api endpoint and begins listening for incoming requests"
  (with-slots (name listen-address handler app) api-endpoint
    (restart-case (when handler (error "API already started"))
      (stop-before-starting ()
        :report "Stop running API and then start new API"
        (stop-api api-endpoint))
      (leave ()
        :report "Leave the current API running"
        (return-from start-api)))
    (log:info "[~a] Starting on ~a" name listen-address)
    (setf handler (clack:clackup app :address host :port port :server server))))

(defun stop-api (api-endpoint)
  "Stops the api endpoint and removes all routes. It's safe to start the API again with the same object after calling this function."
  (with-slots (handler app name) api-endpoint
    (when handler
      (log:info "[~a] Stopping" name)
      (clack:stop handler)
      (setf app (make-instance 'api-app))
      (setf handler nil))))

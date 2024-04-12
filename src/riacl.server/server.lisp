(in-package #:riacl.server)

(defparameter *server-instance* nil "The server node instance.")

(defclass server ()
  ((cluster-manager
    :reader cluster-manager
    :initarg :cluster-manager)
   (data-api
    :reader data-api
    :initarg :data-api)
   (control-api
    :reader control-api
    :initarg :control-api)))

(defmethod print-object ((object server) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~%Cluster: ~a ~%Local Node ID: ~a~%Data API: ~a~%"
            (cluster:cluster-name (cluster-manager object))
            (cluster:cluster-node-id (cluster:local-node (cluster-manager object)))
            (data-api object))))

(define-condition server-already-started (simple-error)
  ((server :initarg :server :reader server))
  (:report (lambda (condition stream)
             (format stream "Server ~A is already started." (server condition)))))

(defun setup-logging ()
  (log:config :file2 :ndc config:*log.level*))

(defun start-server ()
  "Start the server node. It will reload the configuration, initialize all subsystems and begin listening for incoming connections."
  (restart-case (when *server-instance* (error 'server-already-started :server *server-instance*))
    (stop-before-start ()
      :report "Stop the server and then continue"
      (stop-server)))
  (unless *server-instance*
    (config:load-config)
    (setup-logging)
    (unwind-protect
         (let* ((cluster-manager (cluster:manager-for config:*cluster.name*))
                (this-node (cluster:join-this-node cluster-manager)))
           (setf *server-instance* (make-instance 'server
                                                  :cluster-manager cluster-manager
                                                  :data-api (api.data:make-data-api (cluster:cluster-node-id this-node))
                                                  :control-api (api.control:make-control-api (cluster:cluster-node-id this-node))))
           (api:start-api data-api)
           (api:start-api control-api)
           (log:info "[Server] All subsystems started. Cluster name: ~A" config:*cluster.name*)
           *server-instance*)
      (when *server-instance*
        (stop-server)))))

(defun stop-server ()
  "Stop the server node. It will close all connections, stop listening for incoming connections and shutdown all subsystems."
  (when *server-instance*
    (with-slots (data-api control-api cluster-manager) *server-instance*
      (when data-api
        (api:stop-api data-api))

      (when control-api
        (api:stop-api control-api))

      (cluster:leave-this-node cluster-manager)
      (log:info "[Server] All subsystems stopped.")
      (setf *server-instance* nil))))

(in-package #:riacl.server)

(defclass server ()
  ((cluster-manager
    :reader cluster-manager
    :initarg :cluster-manager)
   (data-api
    :reader data-api
    :initarg :data-api)))

(defparameter *server-instance* nil "The server node instance.")

(define-condition server-already-started (simple-error)
  ((server :initarg :server :reader server))
  (:report (lambda (condition stream)
             (format stream "Server ~A is already started." (server condition)))))

(defun start-server ()
  "Start the server node. It will reload the configuration, initialize all subsystems and begin listening for incoming connections."
  (restart-case (when *server-instance* (error 'server-already-started :server *server-instance*))
    (stop-before-start ()
      :report "Stop the server and then continue"
      (stop-server)))

  (unless *server-instance*
    (config:load-config)
    (setup-logging)
    (let ((cluster-manager nil)
          (this-node nil)
          (data-api nil))
      (unwind-protect
           (progn
             (setf cluster-manager (cluster:manager-for config:*cluster.name*)
                   this-node (cluster:join-this-node cluster-manager)
                   data-api (data.api:start-api (cluster:cluster-node-id this-node))
                   *server-instance* (make-instance 'server :cluster-manager cluster-manager :data-api data-api))
             (log:info "[Server] All subsystems started.")
             *server-instance*)
        (unless *server-instance*
          (when data-api (data.api:stop-api data-api))
          (log:info "[Server] All subsystems stopped."))))))

(defun stop-server ()
  "Stop the server node. It will close all connections, stop listening for incoming connections and shutdown all subsystems."
  (when *server-instance*
    (with-slots (data-api cluster-manager) *server-instance*
      (data.api:stop-api data-api)
      (cluster:leave-this-node cluster-manager)
      (log:info "[Server] All subsystems stopped.")
      (setf *server-instance* nil))))

(defun main (&rest args)
  (declare (ignorable args)))

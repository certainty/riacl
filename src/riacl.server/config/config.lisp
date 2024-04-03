(in-package #:riacl.server.config)

(s:defconst +system-path+ (asdf:system-source-directory :riacl/server))
(s:defconst +env-file-pathname+ (asdf:system-relative-pathname :riacl/server ".env"))

(defparameter *log.level* :info "The log level to use")

(defparameter *cluster.name* "riacl" "The name of the cluster")
(defparameter *cluster.vnodes* 3 "The number of vnodes per virtual node")
(defparameter *cluster.seed-nodes* '() "The addresses of the seed nodes for the gossip protocol")
(defparameter *consistency.read-quorum* 2 "The number of replicas to read from before considering a read successful")
(defparameter *consistency.write-quorum* 2 "The number of replicas to write to before considering a write successful")
(defparameter *replication.n_val* 3 "The number of replicas to store for each key")
(defparameter *replication.default-conflict-resolution* :last-write-wins "The conflict resolution strategy to use")

(defvar *api.listen-address* nil "The address to listen for API requests")
(defvar *control.listen-address* nil "The address to listen for control requests." )

(defvar *storage.backend* :memory "The storage backend to use")

(define-condition invalid-configuration-value (error)
  ((name :initarg :name :reader name)
   (value :initarg :value :reader value)
   (message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream "Invalid configuration value ~a: ~a. ~a"
                     (name condition)
                     (value condition)
                     (message condition)))))

(defun as-boolean (value)
  (if (and (stringp value) (member value '("true" "t" "yes" "y" "1")))
      t
      nil))

(defun as-integer (value)
  (let ((parsed (parse-integer value :junk-allowed nil)))
    (if parsed
        parsed
        (error 'invalid-configuration-value :name 'value :value value :message "Not an integer"))))

(defun as-network-address (value)
  (let ((parts (str:split value ":")))
    (if (= (length parts) 2)
        (network:make-network-address (first parts) (as-integer (second parts)))
        (error 'invalid-configuration-value :name 'value :value value :message "Not a network address"))))

(defun as-comma-separated-of (type-fn)
  (lambda (value)
    (let ((parts (str:split value ",")))
      (when (null parts)
        (error 'invalid-configuration-value :name 'value :value value :message "Not a comma separated list"))
      (mapcar type-fn parts))))

(defun as-one-of (options &key (case-sensitive t))
  (lambda (value)
    (if (member value options :test (if case-sensitive 'string= 'string-equal))
        value
        (error 'invalid-configuration-value :name 'value :value value :message "Not a valid value. Must be one of: " options))))

(defun as-log-level (value)
  (let ((level (funcall (as-one-of '("debug" "info" "warn" "error" "off") :case-sensitive nil) value)))
    (intern level :keyword)))

(defun load-env-var (name type-fn &key (default nil default-supplied-p))
  (let ((value (uiop:getenv name)))
    (if value
        (funcall type-fn value)
        (if default-supplied-p
            default
            (error 'invalid-configuration-value :name name :value nil :message "No value found")))))

(defun load-config ()
  (.env:load-env +env-file-pathname+)
  (setf *log.level* (load-env-var "LOG_LEVEL" #'as-log-level :default :debug))

  (setf *api.listen-address* (load-env-var "API_LISTEN_ADDRESS" #'as-network-address :default (network:make-network-address "127.0.0.1" 9999)))
  (setf *control.listen-address* (load-env-var "CONTROL_LISTEN_ADDRESS" #'as-network-address :default (network:make-network-address "127.0.0.1" 8888)))

  (setf *storage.backend* (load-env-var "STORAGE_BACKEND" (as-one-of '("memory" "lmbd" "leveldb")) :default :memory))

  (setf *cluster.name* (load-env-var "CLUSTER_NAME" #'identity :default "riacl"))
  (setf *cluster.vnodes* (load-env-var "CLUSTER_VNODES" #'as-integer :default 3))
  (setf *cluster.seed-nodes* (load-env-var "CLUSTER_SEED_NODES" (as-comma-separated-of #'as-network-address) :default '()))

  (setf *consistency.read-quorum* (load-env-var "CONSISTENCY_READ_QUORUM" #'as-integer :default 2))
  (setf *consistency.write-quorum* (load-env-var "CONSISTENCY_WRITE_QUORUM" #'as-integer :default 2))

  (setf *replication.n_val* (load-env-var "REPLICATION_N_VAL" #'as-integer :default 3))
  (setf *replication.default-conflict-resolution* (load-env-var "REPLICATION_DEFAULT_CONFLICT_RESOLUTION" (as-one-of '("last-write-wins" "first-write-wins")) :default :last-write-wins))

  t)

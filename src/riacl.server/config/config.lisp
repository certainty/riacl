(in-package #:riacl.server.config)

(s:defconst +system-path+ (asdf:system-source-directory :riacl/server))
(s:defconst +env-file-pathname+ (asdf:system-relative-pathname :riacl/server ".env"))

(defparameter *log.level* :info "The log level to use")

(defparameter *cluster.name* "riacl" "The name of the cluster")
(defparameter *cluster.seed-nodes* '() "The seed nodes to use")
(defparameter *cluster.ring-size* 1024 "The size of the ring")
(defparameter *cluster.members* '() "The initial members of the cluster. Must be valid nodenames")

(defparameter *bucket.default.read-quorum* 2 "The number of replicas to read from before considering a read successful")
(defparameter *bucket.default.write-quorum* 2 "The number of replicas to write to before considering a write successful")
(defparameter *bucket.default.replicas* 3 "The number of replicas to store")
(defparameter *bucket.default.conflict-resolution* :last-write-wins "The conflict resolution strategy to use")

(defvar *storage.backend* :memory "The storage backend to use")

(defvar *api.data.listen-address* nil "The address to listen for API requests")
(defvar *api.control.listen-address* nil "The address to listen for control requests." )

(define-condition invalid-configuration-value (error)
  ((name :initarg :name :reader name)
   (detail :initarg :detail :reader detail))
  (:report (lambda (condition stream)
             (format stream "Could not read configuration value for ~a. ==>  ~a"
                     (name condition)
                     (detail condition)))))

(define-condition value-conversion-error (error)
  ((value :initarg :value :reader value)
   (message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream "Error converting value ~a: ~a"
                     (value condition)
                     (message condition)))))

(defun as-boolean (value)
  (s:true (and (stringp value) (member value '("true" "t" "yes" "y" "1") :test #'string-equal))))

(defun as-integer (value)
  (let ((parsed (parse-integer value :junk-allowed nil)))
    (if parsed
        parsed
        (error 'value-conversion-error :value value :message "Not an integer"))))

(defun as-network-address (value)
  (let ((addr (network:parse-address value)))
    (prog1 addr
      (unless addr
        (error 'value-conversion-error :value value :message "Not a network address")))))

(defun as-comma-separated-of (type-fn)
  (lambda (value)
    (let ((parts (str:split "," value :omit-nulls t)))
      (when (null parts)
        (error 'value-conversion-error :value value :message "Not a comma separated list"))
      (mapcar type-fn parts))))

(defun as-one-of (options &key (case-sensitive t))
  (lambda (value)
    (if (member value options :test (if case-sensitive 'string= 'string-equal))
        value
        (error 'value-conversion-error :value value :message "Not a valid value. Must be one of: " options))))

(defun as-log-level (value)
  (let ((level (funcall (as-one-of '("debug" "info" "warn" "error" "off") :case-sensitive nil) value)))
    (intern level :keyword)))

(defun load-env-var (name type-fn &key (default nil default-supplied-p))
  (restart-case
      (if default-supplied-p
          (%load-env-var name type-fn :default default)
          (%load-env-var name type-fn))
    (use-value (value)
      :report "Sepcify the value to use"
      value)))

(defun %load-env-var (name type-fn &key (default nil default-supplied-p))
  (let ((value (uiop:getenv name)))
    (handler-case
        (progn
          (if (and value (not (string= value "")))
              (funcall type-fn value)
              (if default-supplied-p
                  default
                  (error 'invalid-configuration-value :name name :value nil :message "No value found"))))
      (value-conversion-error (e)
        (error 'invalid-configuration-value :name name :detail e)))))

(defun load-config ()
  (.env:load-env +env-file-pathname+)
  (setf *log.level* (load-env-var "LOG_LEVEL" #'as-log-level :default :debug))

  (setf *api.data.listen-address* (load-env-var "API_DATA_LISTEN_ADDRESS" #'as-network-address :default (network:parse-address "127.0.0.1:9999")))
  (setf *api.control.listen-address* (load-env-var "API_CONTROL_LISTEN_ADDRESS" #'as-network-address :default (network:parse-address "127.0.0.1:9998")))

  (setf *storage.backend* (load-env-var "STORAGE_BACKEND" (as-one-of '("memory" "lmbd" "leveldb")) :default :memory))

  (setf *cluster.name* (load-env-var "CLUSTER_NAME" #'identity :default "riacl"))
  (setf *cluster.seed-nodes* (load-env-var "CLUSTER_SEED_NODES" (as-comma-separated-of #'as-network-address) :default '()))

  (setf *bucket.default.read-quorum* (load-env-var "BUCKET_DEFAULT_READ_QUORUM" #'as-integer :default 2))
  (setf *bucket.default.write-quorum* (load-env-var "BUCKET_DEFAULT_WRITE_QUORUM" #'as-integer :default 2))
  (setf *bucket.default.replicas* (load-env-var "BUCKET_DEFAULT_REPLICAS" #'as-integer :default 3))
  (setf *bucket.default.conflict-resolution* (load-env-var "BUCKET_DEFAULT_CONFLICT_RESOLUTION" (as-one-of '("last-write-wins" "first-write-wins")) :default :last-write-wins))

  t)

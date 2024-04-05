(in-package :riacl.server.cluster)

(defclass node ()
  ((name
    :reader node-name
    :initarg :name
    :initform (error "name is required"))
   (id
    :reader node-id
    :initarg :id
    :initform (error "id is required"))))

(defclass physical-node (node)
  ((data-listen-address
    :reader physical-node-data-listen-address
    :initarg :data-listen-address
    :initform (error "data-listen-address is required")
    :type network:network-address)
   (control-listen-address
    :reader physical-node-control-listen-address
    :initarg :control-listen-address
    :initform (error "control-listen-address is required")
    :type network:network-address)))

(defclass virtual-node (node) ())

(deftype membership-state () '(member :joining :leaving :up :exiting :down))

(defclass cluster ()
  ((virtual-nodes
    :reader cluster-virtual-nodes
    :initarg :virtual-nodes
    :initform (make-hash-table :test 'equal)
    :type hash-table)
   (physical-nodes
    :reader cluster-physical-nodes
    :initarg :physical-nodes
    :initform (make-hash-table :test 'equal)
    :type hash-table)))

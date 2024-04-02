(in-package :cl-user)

(defpackage #:riacl.server.cluster
  (:use :cl)
  (:nicknames :server.cluster :cluster)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:cluster-manager
   #:cluster-name
   #:local-node
   #:manager-for
   #:join-this-node
   #:leave-this-node
   #:cluster-node
   #:make-cluster-node
   #:cluster-node-id
   #:cluster-node-address
   #:seed-node
   #:cluster-state
   #:cluster-known-nodes
   #:cluster-seed-nodes
   #:cluster-local-node
   #:make-cluster-state))

(in-package :cl-user)

(defpackage #:riacl.server.cluster.identifier
  (:use :cl #:riacl.prelude)
  (:nicknames :cluster.identifiers :identifier)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:identifier
   #:identifierp
   #:identifier=
   #:identifier-sxhash
   #:kind-of
   #:cluster-name
   #:bucket-name
   #:node-name
   #:vnode-name
   #:object-key))

(defpackage #:riacl.server.cluster.ring
  (:use :cl)
  (:nicknames :server.cluster.ring :cluster.ring :ring)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:consistent-hash-ring))

(defpackage #:riacl.server.cluster.vector-clock
  (:use :cl)
  (:nicknames :vector-clock :vlock)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:shadow :merge)
  (:export
   #:vector-clock
   #:make-vector-clock
   #:merge
   #:descendsp
   #:descends-dot-p
   #:dominatsp
   #:prune))

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

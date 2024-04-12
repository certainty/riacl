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
   #:identifier<
   #:identifier>
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

(defpackage #:riacl.server.cluster.dvv
  (:use :cl)
  (:nicknames :cluster.dvv :dvv)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:shadow :merge)
  (:export
   #:counter
   #:dot
   #:copy-dot
   #:dot=
   #:dot-actor-id
   #:dot-counter
   #:dot-timestamp
   #:dot-actor=
   #:dot-counter=
   #:dot-counter<
   #:dot-counter>
   #:dot-counter>=
   #:dot-counter<=
   #:dotted-version-vector
   #:copy-dotted-version-vector
   #:dotted-version-vector=
   #:dotted-version-vector-history
   #:dotted-version-vector-history-sorted
   #:dotted-version-vector-dot
   #:make-dotted-version-vector
   #:merge*
   #:emptyp
   #:descendsp
   #:descends-dot-p
   #:dominatesp
   ))

(defpackage #:riacl.server.cluster.faults
  (:use :cl)
  (:nicknames :cluster.dvv :dvv)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->))


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

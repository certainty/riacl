(in-package :cl-user)

(defpackage #:riacl.clock
  (:use :cl)
  (:nicknames :clock)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export #:seconds-since-epoch #:seconds-since-epoch/impl #:*universal-clock*))

(defpackage #:riacl.server.config
  (:use :cl)
  (:nicknames :server.config :config)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:load-config
   #:network-host
   #:network-port
   #:*api.control.listen-address*
   #:*api.data.listen-address*
   #:*control.seed-nodes*
   #:*storage.backend*
   #:*cluster.name*
   #:*cluster.node-id*
   #:*log.level*))

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
   #:make-dot
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
   #:incf-dot
   #:dotted-version-vector
   #:copy-dotted-version-vector
   #:dotted-version-vector=
   #:dotted-version-vector-history
   #:dotted-version-vector-history-sorted
   #:dotted-version-vector-dot
   #:make-dotted-version-vector
   #:incf-actor
   #:merge*
   #:emptyp
   #:descendsp
   #:descends-dot-p
   #:dominatesp
   ))

(defpackage #:riacl.server.cluster.faults
  (:use :cl)
  (:nicknames :cluster.faults :faults)
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

(defpackage #:riacl.server.api.foundation
  (:use :cl)
  (:nicknames :api.foundation :api)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:start-api
   #:stop-api
   #:api-endpoint
   #:api-handler
   #:api-name
   #:api-list-address
   #:make-api-endpoint
   #:add-route
   #:add-routes
   #:get-request-header
   #:register-encoder
   #:encode-content
   #:json-encoder
   #:msgpack-encoder
   #:respond-with
   #:respond-with-error))

(defpackage #:riacl.server.api.data
  (:use :cl)
  (:nicknames :api.data)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export #:data-api #:make-data-api))

(defpackage #:riacl.server.api.control
  (:use :cl)
  (:nicknames :api.control)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export #:control-api #:make-control-api))

(defpackage #:riacl.server.data
  (:use :cl)
  (:nicknames :data)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->))

(defpackage #:riacl.server
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export #:start-server #:stop-server))

(defpackage #:riacl.server.cli
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export #:main))

(in-package :cl-user)

(defpackage #:riacl.server.config
  (:use :cl)
  (:nicknames :server.config :config)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:load-config
   #:network-host
   #:network-port
   #:*api.listen-address*
   #:*control.listen-address*
   #:*control.seed-nodes*
   #:*storage.backend*
   #:*cluster.name*
   #:*cluster.node-id*
   #:*log.level*))

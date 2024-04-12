(in-package :cl-user)

(defpackage #:riacl.server
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export #:start-server #:stop-server))


(defpackage #:riacl.clock
  (:use :cl)
  (:nicknames :clock)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export #:seconds-since-epoch #:seconds-since-epoch/impl #:*universal-clock*))

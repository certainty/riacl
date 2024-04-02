(in-package :cl-user)

(defpackage #:riacl.server.config
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->))

(defpackage #:riacl.server
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export #:start-server #:stop-server #:main))

(in-package :cl-user)

(defpackage #:riacl.server.cli
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export #:main))

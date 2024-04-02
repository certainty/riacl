(in-package :asdf-user)

(defsystem "riacl"
  :description "Let's reinvent the wheel! A riak inspired distributed key-value store in Common Lisp."
  :version "0.1.0"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/riacl.git")
  :depends-on (#:riacl/server #:riacl/client))

(defsystem "riacl/server"
  :description "A riak inspired distributed key-value store in Common Lisp."
  :version "0.1.0"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/riacl.git")
  :in-order-to ((test-op (test-op "riacl/server.tests")))
  :defsystem-depends-on (#:deploy)
  :build-operation "deploy-op"
  :build-pathname "riacl"
  :entry-point "riacl.server:main"
  :depends-on (#:serapeum #:alexandria #:ningle #:cl-dotenv #:log4cl)
  :pathname "src/riacl.server"
  :serial t
  :components
  ((:file "package")
   (:file "config")))

(defsystem "riacl/server.tests"
  :description "Unit tests for riacl/server"
  :depends-on (#:lisp-unit2  #:riacl/server)
  :pathname "test/riacl.server"
  :perform (test-op (o c)
                    (declare (ignore o c))
                    (uiop:symbol-call :riacl.server.tests :run-suites))
  :components
  ((:file "package")
   (:file "runner")))

(defsystem "riacl/client"
  :description "A client for riacl."
  :version "0.1.0"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/riacl.git")
  :in-order-to ((test-op (test-op "riacl/client.tests")))
  :pathname "src/riacl.client"
  :serial t
  :components
  ((:file "package")))

(defsystem "riacl/client.tests"
  :description "Unit tests for riacl/client"
  :depends-on (#:lisp-unit2  #:riacl/client)
  :pathname "test/riacl.client"
  :components
  ((:file "package")
   (:file "runner"))
  :perform (test-op (o c)
                    (declare (ignore o c))
                    (uiop:symbol-call :riacl.client.tests :run-suites)))

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
  :depends-on
  (#:riacl/common
   #:serapeum
   #:alexandria

   #:ningle
   #:clack
   #:lack

   #:chanl
   #:trivial-signal

   #:cl-json
   #:cl-messagepack
   #:str
   #:quri

   #:ironclad
   #:fast-io
   #:cl-dotenv
   #:log4cl
   #:local-time)

  :pathname "src/riacl.server"
  :serial t
  :components
  ((:file "package")
   (:file "clock")
   (:module "config"
    :components
    ((:file "package")
     (:file "config")))

   (:module "cluster"
    :components
    ((:file "package")
     (:file "identifiers")
     (:file "dvv")
     (:file "cluster")
     (:file "manager")))

   (:module "data"
    :components
    ((:file "package")))

   (:module "api"
    :components
    ((:file "package")
     (:file "foundation")))

   (:module "api/data"
    :components
    ((:file "package")
     (:file "api")))

   (:module "api/control"
    :components
    ((:file "package")
     (:file "api")))

   (:file "server")
   (:module "cli"
    :components
    ((:file "package")
     (:file "main")))))

(defsystem "riacl/server.tests"
  :description "Unit tests for riacl/server"
  :depends-on (#:lisp-unit2  #:riacl/server)
  :pathname "test/riacl.server"
  :components
  ((:file "package")
   (:file "runner")
   (:module "cluster"
    :components
    ((:file "identifier")
     (:file "dvv"))))
  :perform (test-op (o c)
                    (declare (ignore o c))
                    (uiop:symbol-call :riacl.server.tests :run-suites)))

(defsystem "riacl/common"
  :description "Common code for riacl."
  :depends-on
  (#:serapeum
   #:alexandria
   #:str)
  :serial t
  :pathname "src/riacl.common"
  :in-order-to ((test-op (test-op "riacl/common.tests")))
  :components
  ((:file "package")
   (:file "prelude")
   (:file "network")))

(defsystem "riacl/common.tests"
  :description "Unit tests for riacl/common"
  :depends-on (#:lisp-unit2  #:riacl/common)
  :pathname "test/riacl.common"
  :components
  ((:file "package")
   (:file "runner")
   (:file "network"))
  :perform (test-op (o c)
                    (declare (ignore o c))
                    (uiop:symbol-call :riacl.common.tests :run-suites)))

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

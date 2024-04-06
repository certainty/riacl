(in-package #:riacl.common.tests.network)

(define-test address-happy-path ()
  "Test the happy path of the parse-network-address function."
  (let ((address (network:parse-address "192.168.0.1:80")))
    (assert-equal (network:address-ipv4 address) 3232235521)
    (assert-equal (network:address-port address) 80)))

(define-test conversion-of-dotted-quad ()
  "Test the conversion of a dotted quad to an integer."
  (assert-equal
   "168.1.1.1"
   (network:integer->dotted-quad
    (network:dotted-quad->integer "168.1.1.1"))))

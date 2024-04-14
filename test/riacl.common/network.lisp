(in-package #:riacl.common.tests)

(define-test address-happy-path
    "Test the happy path of the parse-network-address function."
  (let ((address (network:parse-address "192.168.0.1:80")))
    (is = 3232235521 (network:address-ipv4 address))
    (is = 80 (network:address-port address) )))

(define-test conversion-of-dotted-quad
    "Test the conversion of a dotted quad to an integer."
  (is string-equal
      "168.1.1.1"
      (network:integer->dotted-quad
       (network:dotted-quad->integer "168.1.1.1"))))

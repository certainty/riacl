(in-package :riacl.server.tests.cluster)

(defmacro assert-identifier-implementation (identifier equal-identifier different-identifier)
  "Baseline tests for an identifier implementation. The implementation must provide "
  `(progn
     (assert-true (identifier:identifierp ,identifier))
     (assert-true (identifier:identifier= ,identifier ,identifier))
     (assert-true (identifier:identifier= ,identifier ,equal-identifier))
     (assert-true (not (identifier:identifier= ,identifier ,different-identifier)))
     (assert-true (keywordp (identifier:kind-of ,identifier)))
     (assert-no-signal 'error (identifier:identifier-sxhash ,identifier))))

(define-test cluster-name-implements-identifier-interfaces ()
  (assert-identifier-implementation (identifier:cluster-name "test-cluster")
                                    (identifier:cluster-name "test-cluster")
                                    (identifier:cluster-name "test-cluster2")))

(define-test node-name-implements-identifier-interfaces ()
  (assert-identifier-implementation (identifier:node-name 2130706433 "foo")
                                    (identifier:node-name 2130706433 "foo")
                                    (identifier:node-name 2130706433 "bar")))

(define-test vnode-name-implements-identifier-interfaces ()
  (assert-identifier-implementation
   (identifier:vnode-name 2130706433 "foo" 1)
   (identifier:vnode-name 2130706433 "foo" 1)
   (identifier:vnode-name 2130706433 "bar" 1)))

(define-test bucket-name-implements-identifier-interfaces ()
  (assert-identifier-implementation
   (identifier:bucket-name "pets" "dog")
   (identifier:bucket-name "pets" "dog")
   (identifier:bucket-name "plants" "tree")))

(define-test object-key-implements-identifier-interfaces ()
  (assert-identifier-implementation
   (identifier:object-key "pets" "dog" "fido")
   (identifier:object-key "pets" "dog" "fido")
   (identifier:object-key "pets" "dog" "spot")))

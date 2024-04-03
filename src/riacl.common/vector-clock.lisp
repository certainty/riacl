(in-package :riacl.common.vector-clock)
;; TODO: implement vector clocks with dotted version vectors
;; https://medium.com/geekculture/all-things-clock-time-and-order-in-distributed-systems-logical-clocks-in-real-life-2-ad99aa64753

(defclass vector-clock () ())

(defun make-vector-clock (&optional initial-dot)
  "Creates a new dotted vector clock."
  (make-vector-clock))

(defun update (vclock node-id &optional (increment 1))
  vclock)

(defun merge (vc1 vc2) vc1)

(defun dominatesp (dot1 dot2) t)

(defun descendsp (vc1 vc2) t)

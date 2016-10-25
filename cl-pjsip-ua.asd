;;;; cl-pjsip-ua.asd

(asdf:defsystem #:cl-pjsip-ua
  :description "A simple SIP UA using CL-PJSIP library"
  :author "Eugene Zaikonnikov <eugene@funcall.org>"
  :license "GPL v2"
  :depends-on (#:cl-pjsip)
  :serial t
  :components ((:file "cl-pjsip-ua")))


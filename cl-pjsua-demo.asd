;;;; cl-pjsua-demo.asd

(asdf:defsystem #:cl-pjsua-demo
  :description "A simple PJSUA based demo using CL-PJSIP library"
  :author "Eugene Zaikonnikov <eugene@funcall.org>"
  :license "GPL v2"
  :depends-on (#:cl-pjsip)
  :serial t
  :components ((:file "cl-pjsua-demo")))


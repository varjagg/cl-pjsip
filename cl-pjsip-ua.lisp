(in-package #:cl-pjsip)

(use-foreign-library libpj)
(use-foreign-library libpjsip)
(use-foreign-library libpjsip-ua)
(use-foreign-library libpjsua)
(use-foreign-library libpjmedia)
(use-foreign-library libpjmedia-codec)
(use-foreign-library libpjlib-util)

(defconstant +sip-port+ 5060)
(defconstant +rtp-port+ 4000)
(defconstant +max-media-cnt+ 2)

(defparameter *complete* (convert-to-foreign 0 'pj-bool))
(defparameter *endpt* (null-pointer))
(defvar *cp* (foreign-alloc '(:struct pj-caching-pool)))
(defparameter *med-endpt* (null-pointer))
(defvar *med-tpinfo* (make-array +max-media-cnt+ :initial-contents (loop repeat +max-media-cnt+
									    collecting (foreign-alloc '(:struct pjmedia-transport-info)))))
(defvar *med-transport* (make-array +max-media-cnt+ :initial-element (null-pointer)))
(defvar *sock-info* (make-array +max-media-cnt+ :initial-contents (loop repeat +max-media-cnt+
									    collecting (foreign-alloc '(:struct pjmedia-sock-info)))))
;;;Call variables
(defvar *inv* (null-pointer))
(defvar *med-stream* (null-pointer))
;;(defvar *snd-port* (null-pointer))

(defvar *mod-simpleua* (foreign-alloc '(:struct pjsip-module)))

(defcallback on-rx-request pj-bool ((rdata (:pointer (:struct pjsip-rx-data))))
  (unwind-protect (with-foreign-objects ((hostaddr '(:union pj-sockaddr))
			 (local-uri 'pj-str)
			 (dlg '(:pointer (:struct pjsip-dialog)))
			 (local-sdp '(:pointer (:struct pjmedia-sdp-session)))
			 (tdata '(:pointer (:struct pjsip-tx-data))))
    (foreign-slot-value 
     (foreign-slot-value 
      (foreign-slot-value rdata '(:struct pjsip-rx-data) 'msg-info) '(:struct rx-data-msg-info) 'msg)
     '(:struct pjsip-msg) 'line)))
  1)

(defun init ()
  (with-foreign-slots ((name priority on-rx-request) *mod-simpleua* (:struct pjsip-module))
    (setf name "mod-simpleua"
	  priority (foreign-enum-keyword 'pjsip-module-priority :pjsip-module-priority-application)
	  on-rx-request (callback 'on-rx-request))))

(defcvar "pj_pool_factory_default_policy" :pointer)

(defun run-agent ()
  (with-foreign-object (pool-ptr '(:pointer (:struct pj-pool)))
    (let (status)
      (assert (= (pj-init) 0))
      (pj-set-log-level 5)
      (assert (= (pjlib-util-init) 0))
      (pj-caching-pool-init *cp* (get-var-pointer *pj-pool-factory-default-policy*) 0)
      (with-foreign-object (hostname '(:pointer (:struct pj-str)))
	))))

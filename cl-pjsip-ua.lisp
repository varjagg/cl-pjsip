(in-package #:cl-pjsip)

(defconstant +sip-port+ 5060)
(defconstant +rtp-port+ 4000)
(defconstant +max-media-cnt+ 2)
(defconstant +ivp6_addr_size+ 46)

(defparameter *complete* nil)
(defparameter *endpt* (foreign-alloc '(:pointer pjsip-endpoint) :initial-contents (list (null-pointer))))
(defparameter *cp* (foreign-alloc 'pj-caching-pool))
(defparameter *med-endpt* (foreign-alloc '(:pointer pjmedia-endpt) :initial-contents (list (null-pointer))))
(defparameter *med-tpinfo* (foreign-alloc 'pjmedia-transport-info :count +max-media-cnt+))
(defparameter *med-transport* (foreign-alloc '(:pointer pjmedia-transport) :count +max-media-cnt+
				       :initial-contents (loop repeat +max-media-cnt+ collecting (null-pointer))))
(defvar *sock-info* (foreign-alloc 'pjmedia-sock-info :count +max-media-cnt+))
;;;Call variables
(defparameter *inv* (foreign-alloc '(:pointer pjsip-inv-session) :initial-contents (list (null-pointer))))
(defvar *med-stream* (foreign-alloc '(:pointer pjmedia-stream) :initial-contents (list (null-pointer))))
;;(defvar *snd-port* (null-pointer))

(defvar *mod-simpleua* (foreign-alloc 'pjsip-module))

(defun deref (var)
  (mem-ref var :pointer))

(defun load-pjsip-libraries ()
  (pushnew #p"/usr/local/lib" *foreign-library-directories* :test #'equalp)
  (use-foreign-library libpjlib-util)
  (use-foreign-library libpj)
  (use-foreign-library libpjsip)
  (use-foreign-library libpjsua)
  (use-foreign-library libpjsip-ua)
  (use-foreign-library libpjmedia)
  (use-foreign-library libpjmedia-codec)
  (use-foreign-library libpjmedia-audiodev))

(defcallback logger :void ((level :int) (data :string) (len :int))
  (declare (ignorable len level))
  (format t "~A" data))

(defun ua-log (string)
  (pj-log "cl-pjsip-ua.lisp" 1 string))

(defcallback on-rx-request pj-bool ((rdata (:pointer pjsip-rx-data)))
  (unwind-protect
       (ua-log "Handling RX request")
       (with-foreign-objects ((hostaddr 'pj-sockaddr)
			      (local-uri 'pj-str)
			      (dlg '(:pointer pjsip-dialog))
			      (pjs 'pj-str)
			      (options :int)
			      (hostip :char +ivp6_addr_size+)
			      (local-sdp '(:pointer pjmedia-sdp-session))
			      (tdata '(:pointer pjsip-tx-data)))
	 (let ((id-val (foreign-slot-value
			(foreign-slot-pointer
			 (foreign-slot-pointer
			  (foreign-slot-value 
			   (foreign-slot-pointer (foreign-slot-value rdata 'pjsip-rx-data 'msg-info) 'rx-data-msg-info 'msg)
			   'pjsip-msg 'line)
			  'msg-line 'req)
			 'pjsip-request-line 'method)
			'pjsip-method 'id)))
	   (if (not (eql :pjsip-invite-method (foreign-enum-keyword 'pjsip-method-e id-val)))
	       (if (not (eql :pjsip-ack-method (foreign-enum-keyword 'pjsip-method-e id-val)))
		   (progn 
		     (pjsip-endpt-respond-stateless (deref *endpt*) (deref rdata) 500 (lisp-string-to-pj-str "Unable to handle request" pjs)
						    (null-pointer) (null-pointer)))
		   (return-from on-rx-request t)))

	   (unless (null-pointer-p (deref *inv*))
	     (pjsip-endpt-respond-stateless (deref *endpt*) (deref rdata) 500 (lisp-string-to-pj-str "Another call in progress" pjs)
					    (null-pointer) (null-pointer))
	     (return-from on-rx-request t))

	   (unless (pj-success (pjsip-inv-verify-request (deref rdata) options (null-pointer) (null-pointer) *endpt* (null-pointer)))
	     (pjsip-endpt-respond-stateless (deref *endpt*) (deref rdata) 500 (lisp-string-to-pj-str "INVITE too complex for this humble agent" pjs)
					    (null-pointer) (null-pointer))
	     (return-from on-rx-request t))

	   (unless (pj-success (pj-gethostip *pj-af-inet* hostaddr))
	     (ua-log "Unable to retrieve local host IP")
	     (return-from on-rx-request t))
	   (lisp-string-to-pj-str 
	    (format nil "<sip:simpleuas@~A:~D>" (pj-sockaddr-print hostaddr hostip +ivp6_addr_size+ 2) +sip-port+)
	    local-uri)
	   
	   (unless (pj-success (pjsip-dlg-create-uas-and-inc-lock (pjsip-ua-instance) (deref rdata) local-uri dlg))
	     (pjsip-endpt-respond-stateless (deref *endpt*) (deref rdata) 500 (null-pointer)
					    (null-pointer) (null-pointer))
	     (return-from on-rx-request t))
	   
	   (unless (pj-success (pjmedia-endpt-create-sdp (deref *med-endpt*) 
							 (foreign-slot-pointer (foreign-slot-value (deref rdata) 'pjsip-rx-data 'tp-info)
									       'rx-data-tp-info 'pool)
							 +max-media-cnt+ (deref *sock-info*) local-sdp))
	     (pjsip-dlg-dec-lock dlg)
	     (return-from on-rx-request t))
	   
	   (unless (pj-success (pjsip-inv-create-uas dlg (deref rdata) (deref local-sdp) 0 *inv*))
	     (pjsip-dlg-dec-lock dlg)
	     (return-from on-rx-request t))
	   
	   (pjsip-dlg-dec-lock dlg)

	   ;; initial 180 response
	   (unless (pj-success (pjsip-inv-initial-answer (deref *inv*) (deref rdata) 180 (null-pointer) (null-pointer) tdata))
	     (return-from on-rx-request t))

	   (unless (pj-success (pjsip-inv-send-msg (deref *inv*) (deref tdata)))
	     (return-from on-rx-request t))

	   ;; 200 response
	   (unless (pj-success (pjsip-inv-initial-answer (deref *inv*) (deref rdata) 200 (null-pointer) (null-pointer) tdata))
	     (return-from on-rx-request t))

	   (unless (pj-success (pjsip-inv-send-msg (deref *inv*) (deref tdata)))
	     (return-from on-rx-request t)))))
  t)

(defun init ()
  (load-pjsip-libraries)
  (foreign-funcall "bzero" :pointer *mod-simpleua* :int (foreign-type-size 'pjsip-module) :void)
  (with-foreign-slots ((name id priority on-rx-request) *mod-simpleua* pjsip-module)
    (lisp-string-to-pj-str "mod-simpleua" name)
    (setf priority (foreign-enum-value 'pjsip-module-priority :pjsip-mod-priority-application)
	  id -1
	  on-rx-request (callback on-rx-request))))

(defcallback call-on-state-changed :void ((inv (:pointer pjsip-inv-session)) (e (:pointer pjsip-event)))
  (declare (ignorable e))
  (if (eql (foreign-enum-keyword 'pjsip-inv-state (inv-session-state inv)) :pjsip-inv-state-disconnected)
      (progn 
	(ua-log (format nil "Call DISCONNECTED [reason = ~A]" (foreign-enum-keyword 'pjsip-status-code (inv-session-cause inv))))
	(setf *complete* t))
      (ua-log (format nil "Call state changed to ~A" (foreign-enum-keyword 'pjsip-inv-state (inv-session-state inv))))))

(defcallback call-on-forked :void ((inv (:pointer pjsip-inv-session)) (e (:pointer pjsip-event)))
  (declare (ignore inv e)))

(defcallback call-on-media-update :void ((inv (:pointer pjsip-inv-session)) (status pj-status))
  (unwind-protect
       (with-foreign-objects ((stream-info 'pjmedia-stream-info)
			      (local-sdp '(:pointer pjmedia-sdp-session))
			      (remote-sdp '(:pointer pjmedia-sdp-session))
			      (media-port '(:pointer pjmedia-port)))
	 (unless (pj-success status)
	   (ua-log "SDP negotiation failed!")
	   (return-from call-on-media-update))
	 
	 (pjmedia-sdp-neg-get-active-local (inv-session-neg inv) local-sdp)
	 (pjmedia-sdp-neg-get-active-remote (inv-session-neg inv) remote-sdp)
	 
	 (unless (pj-success (pjmedia-stream-info-from-sdp stream-info (foreign-slot-value (inv-session-dlg inv) 'pjsip-dialog 'pool)
							   (deref *med-endpt*) local-sdp remote-sdp 0))
	   (ua-log "Unable to create audio stream info!")
	   (return-from call-on-media-update))
	 
	 (unless (pj-success (pjmedia-stream-create (deref *med-endpt*) (foreign-slot-value (inv-session-dlg inv) 'pjsip-dialog 'pool)
						    stream-info (mem-aref *med-transport* :pointer 0)
						    (null-pointer) *med-stream*))
	   (ua-log "Unable to create audio stream info!")
	   (return-from call-on-media-update))

	 (unless (pj-success (pjmedia-stream-start (deref *med-stream*)))
	   (ua-log "Unable to start audio stream!")
	   (return-from call-on-media-update))
	 
	 (pjmedia-stream-get-port (deref *med-stream*) media-port)
	 
	 ;;could be forwarding media stream to audio dev here
	 )))

(defun run-agent (&optional uri) 
  (unwind-protect			;to unwind and protect!
       (progn
	 (assert-success (pj-init))
	 (pj-log-set-level 5)
	 (pj-log-set-log-func (callback logger))
	 (ua-log "Starting user agent..")
	 (assert-success (pjlib-util-init))
	 (pj-caching-pool-init *cp* (pj-pool-factory-get-default-policy) 0)
	 (let ((endpt-name (machine-instance)))
	   (ua-log (format nil "Initialize SIP endpoint with name ~A" endpt-name))
	   (assert-success (pjsip-endpt-create (foreign-slot-pointer *cp* 'pj-caching-pool 'factory) endpt-name *endpt*)))
	 (with-foreign-object (addr 'pj-sockaddr)
	   (pj-sockaddr-init *pj-af-inet* addr (null-pointer) +sip-port+) ;;ipv4
	   (assert-success (pjsip-udp-transport-start (deref *endpt*) (foreign-slot-pointer addr 'pj-sockaddr 'ipv4)
						      (null-pointer) 1 (null-pointer))))
	 (ua-log "Init transaction layer")
	 (assert-success (pjsip-tsx-layer-init-module (deref *endpt*)))
	 (ua-log "Init UA layer")
	 (assert-success (pjsip-ua-init-module (deref *endpt*) (null-pointer)))

	 (ua-log "Init INVITE session module")
	 (with-foreign-object (inv-cb 'pjsip-inv-callback)
	   (foreign-funcall "bzero" :pointer inv-cb :int (foreign-type-size 'pjsip-inv-callback) :void)
	   (with-foreign-slots ((on-state-changed on-new-session on-media-update) inv-cb pjsip-inv-callback)
	     (setf on-state-changed (callback call-on-state-changed)
		   on-new-session (callback call-on-forked)
		   on-media-update (callback call-on-media-update))
	     (assert-success (pjsip-inv-usage-init (deref *endpt*) inv-cb))))
	 (assert-success (pjsip-100rel-init-module (deref *endpt*)))
	 (assert-success (pjsip-endpt-register-module (deref *endpt*) *mod-simpleua*))

	 (ua-log "Initialize media endpoint")
	 (assert-success (pjmedia-endpt-create (foreign-slot-pointer *cp* 'pj-caching-pool 'factory) (null-pointer) 1 *med-endpt*))

	 (ua-log "initialize G711 codec")
	 (assert-success (pjmedia-codec-g711-init (deref *med-endpt*)))
	 (loop for i from 0 below +max-media-cnt+ do
	      (ua-log (format nil "Create transport endpoint ~D..." i))
	      (assert-success (pjmedia-transport-udp-create3 (deref *med-endpt*) *pj-af-inet*
							     (null-pointer) (null-pointer) (+ (* i 2) +rtp-port+)
							     0 (mem-aptr *med-transport* '(:pointer pjmedia-transport) i)))

	      (pjmedia-transport-info-init (mem-aptr *med-tpinfo* 'pjmedia-transport-info i))
	      (pjmedia-transport-get-info (deref (mem-aptr *med-transport* '(:pointer pjmedia-transport) i))
					  (mem-aptr *med-tpinfo* 'pjmedia-transport-info i))

	      (foreign-funcall "memcpy" :pointer (mem-aptr *sock-info* 'pjmedia-sock-info i)
			       :pointer (foreign-slot-pointer (mem-aptr *med-tpinfo* 'pjmedia-transport-info i)
							      'pjmedia-transport-info 'sock-info)
			       :int (foreign-type-size 'pjmedia-sock-info)
			       :void)
	      (ua-log "    ..done!"))

	 (if uri
	     (with-foreign-objects ((hostaddr 'pj-sockaddr)
				    (dst-uri 'pj-str)
				    (local-uri 'pj-str)
				    (dlg '(:pointer pjsip-dialog))
				    (local-sdp '(:pointer pjmedia-sdp-session))
				    (tdata '(:pointer pjsip-tx-data)))
	       (unless (pj-success (pj-gethostip *pj-af-inet* hostaddr))
		 (ua-log "Unable to retrieve local host IP")
		 (return-from run-agent nil))
	       (lisp-string-to-pj-str uri local-uri)
	       (lisp-string-to-pj-str (format nil "<sip:simpleuac@~A:~D>" (pj-str-to-lisp hostaddr) +sip-port+) local-uri)
	       ;;create UAC dialog
	       (assert-success (pjsip-dlg-create-uac (pjsip-ua-instance) local-uri local-uri dst-uri dst-uri dlg))
	       (assert-success (pjmedia-endpt-create-sdp (deref *med-endpt*)
							 (foreign-slot-pointer (mem-ref dlg 'pjsip-dialog) 'pjsip-dialog 'pool)
							 +max-media-cnt+ *sock-info* local-sdp))

	       ;;create the INVITE session and pass the above created SDP
	       (assert-success (pjsip-inv-create-uac dlg local-sdp 0 *inv*))

	       (assert-success (pjsip-inv-invite (deref *inv*) tdata))
	       ;;get the ball rolling over the net
	       (assert-success (pjsip-inv-send-msg (deref *inv*) tdata)))
	     (ua-log "Ready to accept incoming calls.."))
      
	 (loop until *complete* do
	      (with-foreign-object (timeout 'pj-time-val)
		(setf (foreign-slot-value timeout 'pj-time-val 'sec) 0
		      (foreign-slot-value timeout 'pj-time-val 'msec) 10)
		(pjsip-endpt-handle-events (deref *endpt*) timeout))))
    (ua-log "Shutting down..")
    (unless (null-pointer-p (deref *med-stream*))
      (pjmedia-stream-destroy (deref *med-stream*)))
    
    ;;destroy media transports, deinit endpoints..
    (loop for i from 0 below +max-media-cnt+ do
	 (unless (null-pointer-p (deref (mem-aptr *med-transport* '(:pointer pjmedia-transport) i)))
	   (pjmedia-transport-close (deref (mem-aptr *med-transport* '(:pointer pjmedia-transport) i)))))
    
    (unless (null-pointer-p (deref *med-endpt*))
      (pjmedia-endpt-destroy (deref *med-endpt*)))
    
    (unless (null-pointer-p (deref *endpt*))
      (pjsip-endpt-destroy (deref *endpt*))))
  t)


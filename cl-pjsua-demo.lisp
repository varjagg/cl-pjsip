(in-package #:cl-pjsip)

(defvar *sip-domain* "10.0.14.3")
(defvar *sip-user* "1100")
(defvar *sip-passwd* "secret")

(defconstant +pjsip-cred-data-plain-passwd+ 0)

(defmacro assert-no-error (form &optional error-message)
  (let ((retval (gentemp)))
    (unless error-message
      (setf error-message (format nil "Error in ~A" (symbol-name (first form)))))
    `(let ((,retval ,form))
       (unless (pj-success ,retval)
	 (error-exit ,error-message ,retval)))))

(defcallback on-incoming-call :void ((acc-id pjsua-acc-id) (call-id pjsua-call-id) (rdata (:pointer pjsip-rx-data)))
  (declare (ignorable acc-id rdata))
  (with-foreign-object (ci 'pjsua-call-info)
    (pjsua-call-get-info call-id ci)
    (ua-log (format nil "Incoming call from ~A!!" (pj-str-to-lisp (foreign-slot-pointer ci 'pjsua-call-info 'remote-info))))
    (pjsua-call-answer call-id 200 (null-pointer) (null-pointer))))

(defcallback on-call-state :void ((call-id pjsua-call-id) (e (:pointer pjsip-event)))
  (declare (ignorable e))
  (with-foreign-object (ci 'pjsua-call-info)
    (pjsua-call-get-info call-id ci)
    (format t "Call ~D state=~A~%" call-id (foreign-slot-value ci 'pjsua-call-info 'state))))

(defcallback on-call-media-state :void ((call-id pjsua-call-id))
  (with-foreign-object (ci 'pjsua-call-info)
    (pjsua-call-get-info call-id ci)
    (when (eql (foreign-slot-value ci 'pjsua-call-info 'media-status) :pjsua-call-media-active)
      ;; plug the audio in
      (pjsua-conf-connect (foreign-slot-value ci 'pjsua-call-info 'conf-slot) 0)
      (pjsua-conf-connect 0 (foreign-slot-value ci 'pjsua-call-info 'conf-slot)))))

(defun error-exit (title status)
  (pjsua-perror "cl-pjsua-demo" title status)
  (pjsua-destroy)
  (throw 'demo-error nil))

(defun run-pjsua (&optional destination)
  (let ((acc-id (foreign-alloc 'pjsua-acc-id)))
    (use-foreign-library libpjsua)

    (assert-no-error (pjsua-create))
  
    (when destination
      (assert-no-error (pjsua-verify-url destination) "Invalid URL as argument"))
  
    (with-foreign-objects ((cfg 'pjsua-config) (log-cfg 'pjsua-logging-config))
      (pjsua-config-default cfg)
      (with-foreign-slots ((on-incoming-call on-call-media-state on-call-state) (foreign-slot-value cfg 'pjsua-config 'cb) pjsua-callback)
	(setf on-incoming-call (callback on-incoming-call)
	      on-call-media-state (callback on-call-media-state)
	      on-call-state (callback on-call-state)))
    
      (pjsua-logging-config-default log-cfg)
      (with-foreign-slots ((console-level cb) log-cfg pjsua-logging-config)
	(setf console-level 4
	      cb (callback logger)))
    
      (assert-no-error (pjsua-init cfg log-cfg (null-pointer))))
  
    (with-foreign-object (cfg 'pjsua-transport-config)
      (pjsua-transport-config-default cfg)
      (setf (foreign-slot-value cfg 'pjsua-transport-config 'port) 5060)
      (assert-no-error (pjsua-transport-create :pjsip-transport-udp cfg (null-pointer)) "Error creating transport"))
  
    (assert-no-error (pjsua-start) "Error starting pjsua")

    (with-foreign-object (cfg 'pjsua-acc-config)
      (pjsua-acc-config-default cfg)
      (with-foreign-slots ((id reg-uri cred-count) cfg pjsua-acc-config)
	(pj-cstr id (format nil "sip:~A@~A" *sip-user* *sip-domain*))
	;;(pj-cstr reg-uri (format nil "sip:~A" *sip-domain*))
	(setf cred-count 1)
	(with-foreign-slots ((realm scheme username data-type data)
			     (mem-aptr (foreign-slot-pointer cfg 'pjsua-acc-config 'cred-info) 'pjsip-cred-info 0)
			     pjsip-cred-info)
	  (pj-cstr realm *sip-domain*)
	  (pj-cstr scheme "digest")
	  (pj-cstr username *sip-user*)
	  (setf data-type +pjsip-cred-data-plain-passwd+)
	  (pj-cstr data *sip-passwd*)
	  (assert-no-error (pjsua-acc-add cfg +pj-true+ acc-id) "Error adding account"))))

    (when destination
      (with-foreign-object (uri 'pj-str)
	(pj-cstr uri destination)
	(assert-no-error (pjsua-call-make-call (mem-ref acc-id 'pjsua-acc-id) uri (null-pointer) 
						(null-pointer) (null-pointer) (null-pointer))
			 "Error making call")))

    (loop for banner = (format t "Press H to hang up all calls, Q to quit~%")
       for option = (read-line)
       when (eql (char-downcase (aref option 0)) #\q) return nil
       else when (eql (char-downcase (aref option 0)) #\h) do
	 (pjsua-call-hangup-all))
    
    (pjsua-destroy)

    (catch 'demo-error
      (foreign-free acc-id)
      #+nil(ua-log "Exiting"))))

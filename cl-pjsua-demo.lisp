(in-package #:cl-pjsip)

(defvar *sip-domain* "example.com")
(defvar *sip-user* "alice")
(defvar *sip-passwd* "secret")

(defmacro assert-no-error (form &optional error-message)
  (let ((retval (gentemp)))
    (unless error-message
      (setf error-message (format nil "Error in ~A" (symbol-name (first form)))))
    `(let ((,retval ,form))
       (unless (pj-success ,retval)
	 (error-exit ,error-message ,retval)))))

(defun error-exit (title status)
  (pjsua-perror "cl-pjsua-demo" title status)
  (pjsua-destroy)
  (throw 'demo-error nil))

(defun run-pjsua (&optional destination)
  (with-foreign-object (acc-id 'pjsua-acc-id)
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
  
    (with-foreign-object (cfg 'pjsua-acc-config)
      (pjsua-acc-config-default cfg)
      (with-foreign-slots ((id reg-uri cred-count cred-info) cfg pjsua-acc-config)
	(setf id (pj-str (format nil "sip:~A@~A" *sip-user* *sip-domain*))
	      reg-uri (pj-str (format nil "sip:~A" *sip-domain*))
	      cred-count 1)
	(with-foreign-slots ((realm scheme username data-type data) (mem-aref cred-info 'pjsip-cred-info 0)
			     pjsip-cred-info)
	  (setf realm (pj-str *sip-domain*)
		scheme (pj-str "digest")
		username (pj-str *sip-user*)
		data-type :pjsip-cred-data-plain-passwd
		data (pj-str *sip-passwd*))
	  (assert-no-error (pjsua-acc-add cfg +pj-true+ acc-id) "Error adding account"))))

    (when destination
      (with-foreign-object (uri 'pj-str)
	(pj-cstr uri destination)
	(assert-no-error (pjsua-call-make-call (deref acc-id) uri 0 
						(null-pointer) (null-pointer) (null-pointer))
			 "Error making call")))

    (loop for banner = (format t "Press H to hang up all calls, Q to quit~%")
       for option = (read-line)
       when (eql (char-downcase (aref option 0)) #\q) return nil
       else when (eql (char-downcase (aref option 0)) #\h) do
	 (pjsua-call-hangup-all))
    
    (pjsua-destroy)

    (catch 'demo-error
      #+nil(ua-log "Exiting"))))

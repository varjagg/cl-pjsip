(in-package #:cl-pjsip)

(defvar *sip-domain* "example.com")
(defvar *sip-user* "alice")
(defvar *sip-passwd* "secret")

(defmacro assert-no-error (form error-message)
  (let ((retval (gentemp)))
    `(let ((,retval ,form))
       (unless (pj-success ,retval)
	 (error-exit ,error-message ,retval)))))

(defun error-exit (title status)
  (pjsua-perror "cl-pjsua-demo" title status)
  (pjsua-destroy)
  (throw 'demo-error nil))

(defun run-pjsua (&optional destination)
  (use-foreign-library libpjsua)

  (assert-no-error (pjsua-create) "Error in pjsua_create()")
  
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
	    cb (callback logger))))
  
  (catch 'demo-error
    (ua-log "Exiting")))

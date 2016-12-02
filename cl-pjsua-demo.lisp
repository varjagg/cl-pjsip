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
    )
  
  (catch 'demo-error
    (ua-log "Exiting")))

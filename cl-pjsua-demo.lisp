(in-package #:cl-pjsip)

(defvar *sip-domain* "example.com")
(defvar *sip-user* "alice")
(defvar *spi-passwd* "secret")

(defun run-pjsua (&optional destination)
  (use-foreign-library libpjsua))

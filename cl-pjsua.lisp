;;; PJSUA high level API wrappers

(in-package #:cl-pjsip)

(defctype pjssua-acc-id :int)

(defcenum pjsua-100rel-use
  :pjsua-100rel-not-used
  :pjsua-100rel-mandatory
  :pjsua-100rel-optional)

(defcenum pjsua-sip-timer-use
  :pjsua-sip-timer-inactive
  :pjsua-sip-timer-optional
  :pjsua-sip-timer-required
  :pjsua-sip-timer-always)

(defcstruct pjsip-timer-setting
  (min-se :uint)
  (sess-expires :uint))

(defcstruct cred-info-aka
  (k pj-str)
  (op pj-str)
  (amf pj-str)
  (cb :pointer))

(defcstruct pjsip-cred-info
  (realm pj-str)
  (scheme pj-str)
  (data-type :int)
  (data pj-str)
  (ext-aka (:struct cred-info-aka)))

(defcenum pjmedia-srtp-use
  :pjmedia-srtp-disabled
  :pjmedia-srtp-optional
  :pjmedia-srtp-mandatory)

(defcstruct pjsua-config
  (max-calls :uint)
  (thread-cnt :uint)
  (nameserver-count :uint)
  (nameserver pj-str :count 4)
  (force-lr pj-bool)
  (outbound-proxy-cnt :uint)
  (stun-domain pj-str)
  (stun-host pj-str)
  (stun-srv-cnt :uint)
  (stun-srv pj-str :count 8)
  (stun-ignore-failure pj-bool)
  (stun-map-use-stun2 pj-bool)
  (nat-type-in-sdp :int)
  (require-100rel pjsua-100rel-use)
  (use-timer pjsua-sip-timer-use)
  (enable-unsolicited-mwi pj-bool)
  (timer-setting (:struct pjsip-timer-setting))
  (cred-count :uint)
  (cred-info (:struct pjsip-cred-info) :count 8) ;PJSUA_ACC_MAX_PROXIES
  (cb :pointer)
  (user-agent pj-str)
  (use-srtp pjmedia-strp-use)
  (srtp-secure-signaling :int)
  (srtp-optional-dup-offer pj-bool)
  (hangup-forked-call pj-bool))


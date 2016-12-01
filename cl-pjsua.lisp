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

(defctype pjsua-config (:struct pjsua-config))

(defctype pjsua-call-id :int)
(defctype pjsua-acc-id :int)
(defctype pjsua-buddy-id :int)
(defctype pjsua-player-id :int)
(defctype pjsua-recorder-id :int)
(defctype pjsua-conf-port-id :int)

(defcstruct pjsua-call-setting
  (flag :uint)
  (req-keyframe-method :uint)
  (aud-cnt :uint)
  (vid-cnt :uint))

(defcenum pjsua-call-media-status
  :pjsua-call-media-none
  :pjsua-call-media-active
  :pjsua-call-media-local-hold
  :pjsua-call-media-remote-hole
  :pjsua-call-media-error)

(defcstruct call-media-info-stream-aud
  (conf-slot pjsua-conf-port-id))

(defctype pjsua-vid-win-id :int)

(defcstruct call-media-info-stream-vid
  (win-in pjsua-vid-win-id)
  (cap-dev pjmedia-vid-dev-index))

(defcunion call-media-info-stream
  (aud (:struct call-media-info-stream-aud))
  (vid (:struct call-media-info-stream-vid)))

(defcstruct pjsua-call-media-info
  (index :uint)
  (type pjmedia-type)
  (dir pjmedia-dir)
  (status pjsua-call-media-status)
  (stream (:union call-media-info-stream)))

(defcstruct call-info-buf
  (local-info :char :count 128)
  (local-contact :char :count 128)
  (remote-info :char :count 128)
  (remote-contact :char :count 128)
  (call-id :char :count 128)
  (last-status-text :char :count 128))

(defcstruct pjsua-call-info
  (id pjsua-call-id)
  (role pjsip-role-e)
  (acc-id pjsua-acc-id)
  (local-info pj-str)
  (local-contact pj-str)
  (remote-info pj-str)
  (remote-contact pj-str)
  (call-id pj-str)
  (setting (:struct pjsua-call-setting))
  (state pjsip-inv-state)
  (state-text pj-str)
  (last-status pjsip-status-code)
  (last-status-text pj-str)
  (media-status pjsua-call-media-status)
  (media-dir pjmedia-dir)
  (conf-slot pjsua-conf-port-id)
  (media-cnt :uint)
  (media pjsua-call-media-info :count 16) ;PJMEDIA_MAX_SDP_MEDIA
  (prov-media-cnt :uint)
  (prov-media pjsua-call-media-info :count 16) ;PJMEDIA_MAX_SDP_MEDIA
  (connect-duration pj-time-val)
  (total-duration pj-time-val)
  (rem-offerer pj-bool)
  (rem-aud-cnt :uint)
  (rem-vid-cnt :uint)
  (buf_ (:struct call-info-buf)))

(defctype pjsua-call-info (:struct pjsua-call-info))

(defcstruct pjsua-logging-config
  (msg-logging pj-bool)
  (level :uint)
  (console-level :uint)
  (decor :uint)
  (log-filename pj-str)
  (log-file-flags :uint)
  (cb :pointer))

(defctype pjsua-logging-config (:struct pjsua-logging-config))


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

(defcstruct pjsua-transport-config
  (port :uint)
  (port-range :uint)
  (public-addr pj-str)
  (bound-add pj-str)
  (tls-setting (:struct pjsip-tls-setting))
  (qos-type pj-qos-type)
  (qos-params pj-qos-params)
  (sockopt-params pj-sockopt-params))

(defctype pjsua-transport-config (:struct pjsua-transport-config))

(defctype pjsua-transport-id :int)

(defcenum pjsua-stun-use
  :pjsua-stun-use-default
  :pjsua-stun-use-disabled
  :pjsua-stun-use-retry-on-failure)

(defcenum pjsua-ipv6-use
  :pjsua-ipv6-disabled
  :pjsua-ipv6-enabled)

(defcenum pjsua-ice-config-use
  :pjsua-ice-config-use-default
  :pjsua-ice-config-use-custom)

(defcenum pjsua-turn-config-use
  :pjsua-trun-config-use-default
  :pjsua-trun-config-use-custom)

(defcstruct pj-ice-sess-options
  (aggressive pj-bool)
  (nominated-check-delay :uint)
  (controlled-agent-want-nom-timeout :int))

(defcstruct pjsua-ice-config
  (enable-ice pj-bool)
  (ice-max-host-cands :int)
  (ice-opt (:struct pj-ice-sess-options))
  (ice-no-rtcp pj-bool)
  (ice-always-update pj-bool))

(defctype pjsua-ice-config (:struct pjsua-ice-config))

(defcstruct pjsua-turn-config
  (enable-turn pj-bool)
  (turn-server pj-str)
  (turn-conn-type pj-turn-tp-type)
  (turn-auth-cred pj-stun-auth-cred))

(defctype pjsua-turn-config (:struct pjsua-turn-config))

(defcenum pjsua-call-hold-type
  :pjsua-call-hold-type-rfc3264
  :pjsua-call-hold-type-rfc2543)

(defcstruct pjsua-acc-config
  (user-data (:pointer :void))
  (priority :int)
  (id pj-str)
  (reg-uri pj-str)
  (reg-hdr-list pjsip-hdr)
  (sub-hdr-list pjsip-hdr)
  (mwi-enabled pj-bool)
  (mwi-expires :uint)
  (publish-enabled pj-bool)
  (publish-opt (:struct pjsip-publishc-opt))
  (unpublsih-max-wait-time-msec :uint)
  (auth-pref pjsip-auth-clt-pref)
  (pidf-tuple-id pj-str)
  (force-contact pj-str)
  (contact-params pj-str)
  (contact-uri-params pj-str)
  (require-100rel pjsua-100rel-use)
  (use-timer pjsua-sip-timer-use)
  (timer-setting (:struct pjsip-timer-setting))
  (proxy-cnt :uint)
  (proxy pj-str :count 8) ;PJSUA_ACC_MAX_PROXIES
  (lock-codec :uint)
  (reg-timeout :uint)
  (reg-delay-before-refresh :uint)
  (unreg-timeout :uint)
  (cred-count :uint)
  (cred-info (:struct pjsip-cred-info) :count 8) ;PJSUA_ACC_MAX_PROXIES
  (transport-id pjsua-transport-id)
  (allow-contact-rewrite pj-bool)
  (contact-rewrite-method :int)
  (contact-use-src-port pj-bool)
  (allow-via-rewrite pj-bool)
  (allow-sdp-nat-rewrite pj-bool)
  (use-rfc5626 :uint)
  (rfc5626-instance-id pj-str)
  (rfc5626-reg-id pj-str)
  (ka-interval :uint)
  (ka-data pj-str)
  (vid-in-auto-show pj-bool)
  (vid-out-auto-transmit pj-bool)
  (vid-wnd-flags :uint)
  (vid-cap-dev pjmedia-vid-dev-index)
  (vid-rend-dev pjmedia-vid-dev-index)
  (vid-stream-rc-config pjmedia-vid-stream-rc-config)
  (rtp-cfg (:struct pjsua-transport-config))
  (ipv6-media-use pjsua-ipv6-use)
  (sip-stun-use pjsua-stun-use)
  (media-stun-use pjsua-stun-use)
  (ice-cfg-use pjsua-ice-config-use)
  (ice-cfg pjsua-ice-config)
  (turn-cfg-use pjsua-turn-config-use)
  (turn-cfg pjsua-turn-config)
  (use-rtp pjmedia-srtp-use)
  (srtp-secure-signaling :int)
  (srtp-optional-dup-offer pj-bool)
  (reg-retry-interval :uint)
  (reg-first-retry-interval :uint)
  (reg-retry-random-interval :uint)
  (drop-calls-on-reg-fail pj-bool)
  (reg-use-proxy :uint)
  (use-stream-ka pj-bool)
  (call-hold-type pjsua-call-hold-type)
  (register-on-acc-add pj-bool))

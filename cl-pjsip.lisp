;;;; Library wrapper

(in-package #:cl-pjsip)

(define-foreign-library libpjsip
  (:unix (:or "libpjsip.so.2" "libpjsip.so")))

(use-foreign-library libpjsip)

(defctype size :unsigned-int)
(defctype pj-status :int)

(defcfun "pj_init" pj-status)
(defcfun "pj_log_set_level" :void (log-level :int))
(defcfun "pjlib_util_init" pj-status)

(defctype pj-size-t size)

(defcstruct pj-pool-factory-policy
  ;;pjsip's own callbackery, we're not going to disturb this
  (block-alloc :pointer)
  (block-free :pointer)
  (callback :pointer))

(defcstruct pj-pool-factory
  (policy (:struct pj-pool-factory-policy))
  ;;pjsip's own callbackery, we're not going to disturb this
  (create-pool :pointer)
  (release-pool :pointer)
  (dump-status :pointer)
  (on-block-alloc :pointer)
  (on-block-free :pointer))

(defcstruct pj-list
  (prev (:pointer :void))
  (next (:pointer :void)))

(defcstruct pj-caching-pool
  (factory (:struct pj-pool-factory))
  (capacity pj-size-t)
  (max-capacity pj-size-t)
  (used-count pj-size-t)
  (used-size pj-size-t)
  (peak-used-size pj-size-t)
  (free-list (:struct pj-list) :count 16)
  (used-list (:struct pj-list))
  (pool-buf :char :count 256))

(defcfun "pj_caching_pool_init" :void (cp (:pointer (:struct pj-caching-pool))) 
	 (factory-default-policy (:pointer (:struct pj-pool-factory-policy))) (max-capacity size))


;;only found as forward decl in sip_types.h
(defcstruct pjsip-endpoint
  )

(defcfun "pjsip_endpt_create" pj-status (factory (:pointer (:struct pj-pool-factory))) (endpt-name :string) 
	 (endpoint (:pointer (:struct pjsip-endpoint))))

(defcunion pj-in6-addr
  (s6-addr :uint8 :count 32)
  (u6-addr32 :uint32 :count 4))

(defcstruct pj-sockaddr-in6
  (sin6-zero-len :uint8)
  (sin6-family :uint8)
  ;;assume zero len
  ;;(sin6-family :uint16)
  (sin6-port :uint16)
  (sin6-flowinfo :uint32)
  (sin6-addr (:union pj-in6-addr))
  (sin6-scope-id :uint32))

(defcstruct pj-in-addr
  (s-addr :uint32))

(defcstruct pj-sockaddr-in
  (sin-zero-len :uint8)
  (sin-family :uint8)
  ;;assume zero len
  ;;(sin6-family :uint16)
  (sin-port :uint16)
  (sin-addr (:struct pj-in-addr))
  (sin-zero :char :count 8))

(defcstruct pj-addr-hdr
  (sa-zero-len :uint8)
  (sa-family :uint8))

(defcunion pj-sockaddr
  (addr (:struct pj-addr-hdr))
  (ipv4 (:struct pj-sockaddr-in))
  (ipv6 (:struct pj-sockaddr-in6)))

(defcfun "pjsip_sockaddr_init" :void (family :int) (addr (:pointer (:union pj-sockaddr))) (cp :pointer) (port :uint))

(defcstruct pjsip-host-port
  (host :string)
  (port :int))

;;forward decl again
(defcstruct pjsip-transport)

(defcfun "pjsip_udp_transport_start" pj-status (endpoint (:pointer (:struct pjsip-endpoint)))
	 (local-addr (:pointer (:struct pj-sockaddr-in)))
	 (hostport (:pointer (:struct pjsip-host-port)))
	 (async-cnt :uint) (p_transport (:pointer (:pointer (:struct pjsip-transport)))))

(defcfun "pjsip_udp_transport_start6" pj-status (endpoint (:pointer (:struct pjsip-endpoint)))
	 (local-addr (:pointer (:struct pj-sockaddr-in6)))
	 (hostport (:pointer (:struct pjsip-host-port)))
	 (async-cnt :uint) (p_transport (:pointer (:pointer (:struct pjsip-transport)))))

(defcfun "pjsip_tsx_layer_init_module" pj-status (endpoint (:pointer (:struct pjsip-endpoint))))

(defcstruct pjsip-ua-init-param
  ;; yet another callback stub
  (on-dlg-forked :pointer))

(defcfun "pjsip_ua_init_module" pj-status (endpoint (:pointer (:struct pjsip-endpoint))) 
	 (prm (:pointer (:struct pjsip-ua-init-param))))

(defcfun "pj_bzero" :void (dst (:pointer :void)) (size pj-size-t))

(defcfun "pjsip_100rel_init_module" pj-status (endpoint (:pointer (:struct pjsip-endpoint))))

(defcstruct pj-str
  (ptr (:pointer :char))
  (slen :long))

(defcstruct pjsip-module
  ;;pj-list really via c macrology originally
  (prev (:pointer :void))
  (next (:pointer :void))
  (name (:struct pj-str))
  (id :int)
  (priority :int)
  ;;callbackery..
  (load :pointer)
  (start :pointer)
  (stop :pointer)
  (unload :pointer)
  (on-rx-request :pointer)
  (on-rx-response :pointer)
  (on-tx-request :pointer)
  (on-tx-response :pointer)
  (on-tsx-state :pointer))

(defcfun "pjsip_endpt_register_module" pj-status (endpoint (:pointer (:struct pjsip-endpoint)))
	 (module (:pointer (:struct pjsip-module))))

(defcstruct pjsip-inv-callback
  (on-state-changed :pointer)
  (on-new-session :pointer)
  (on-tsx-state-changed :pointer)
  (on-rx-offer :pointer)
  (on-rx-reinvite :pointer)
  (on-create-offer :pointer)
  (on-media-update :pointer)
  (on-send-ack :pointer)
  (on-redirected :pointer))

;;opaque type
(defcstruct pjmedia-endpnt
  )

(defcfun "pjmedia_endpt_create" pj-status (factory (:pointer (:struct pj-pool-factory))) (ioqueue :pointer) 
	 (worker-cnt :uint) (endpoint (:pointer (:struct pjmedia-endpnt))))

(defcfun "pjmedia_codec_g711_init" pj-status (endpoint (:pointer (:struct pjmedia-endpnt))))

(defcenum pjmedia-transport-type
  :pjmedia-transport-type-udp
  :pjmedia-transport-type-ice
  :pjmedia-transport-type-srtp
  :pjmedia-transport-type-user)

(defcstruct pjmedia-transport-op
  ;;callbackery
  (get-info :pointer)
  (attach :pointer)
  (detach :pointer)
  (send-rtp :pointer)
  (send-rtcp :pointer)
  (send-rtcp2 :pointer)
  (media-create :pointer)
  (encode-sdp :pointer)
  (media-start :pointer)
  (media-stop :pointer)
  (simulate-lost :pointer)
  (destroy :pointer))

(defcstruct pjmedia-transport
  (name :char :count 32)
  (type pjmedia-transport-type)
  (op (:pointer (:struct pjmedia-transport-op)))
  (user-data (:pointer :void)))

(defcfun "pjmedia_transport_udp_create3" pj-status (endpoint (:pointer (:struct pjmedia-endpnt))) (af :int) (name :string) (addr (:struct pj-str))
	 (port :int) (options :uint) (p-tp (:pointer (:pointer (:struct pjmedia-transport)))))

(defctype pj-sock :long)

(defcstruct pjmedia-sock-info
  (rtp-sock pj-sock)
  (rtp-addr-name (:union pj-sockaddr))
  (rtcp-sock pj-sock)
  (rtcp-addr-name (:union pj-sockaddr)))

(defcstruct pjmedia-transport-specific-info
  (type pjmedia-transport-type)
  (cbsize :int)
  (buffer :char :count 144)) ;36 * sizeof(long)

(defcstruct pjmedia-transport-info
  (sock-info (:struct pjmedia-sock-info))
  (src-rtp-name (:union pj-sockaddr))
  (src-rtcp-name (:union pj-sockaddr))
  (specific-info-cnt :uint)
  (spc-info (:struct pjmedia-transport-specific-info) :count 4))

(defcfun "pjmedia_transport_info_init" :void (info (:pointer (:struct pjmedia-transport-info))))

(defcfun "pjmedia-transport-get-info" pj-status (tp (:pointer (:struct pjmedia-transport))) (info (:pointer (:struct pjmedia-transport-info))))

(defctype pjsip-user-agent (:struct pjsip-module))
(defctype pj-bool :int)

(defcenum pjsip-dialog-state
  :pjsip-dialog-state-null
  :pjsip-dialog-state-established)

(defcstruct pjsip-uri
  (vptr :pointer))

(defcenum pjsip-status-code
  (:pjsip_sc_trying  100)
  (:pjsip_sc_ringing  180)
  (:pjsip_sc_call_being_forwarded  181)
  (:pjsip_sc_queued  182)
  (:pjsip_sc_progress  183)
  (:pjsip_sc_ok  200)
  (:pjsip_sc_accepted  202)
  (:pjsip_sc_multiple_choices  300)
  (:pjsip_sc_moved_permanently  301)
  (:pjsip_sc_moved_temporarily  302)
  (:pjsip_sc_use_proxy  305)
  (:pjsip_sc_alternative_service  380)
  (:pjsip_sc_bad_request  400)
  (:pjsip_sc_unauthorized  401)
  (:pjsip_sc_payment_required  402)
  (:pjsip_sc_forbidden  403)
  (:pjsip_sc_not_found  404)
  (:pjsip_sc_method_not_allowed  405)
  (:pjsip_sc_not_acceptable  406)
  (:pjsip_sc_proxy_authentication_required  407)
  (:pjsip_sc_request_timeout  408)
  (:pjsip_sc_gone  410)
  (:pjsip_sc_request_entity_too_large  413)
  (:pjsip_sc_request_uri_too_long  414)
  (:pjsip_sc_unsupported_media_type  415)
  (:pjsip_sc_unsupported_uri_scheme  416)
  (:pjsip_sc_bad_extension  420)
  (:pjsip_sc_extension_required  421)
  (:pjsip_sc_session_timer_too_small  422)
  (:pjsip_sc_interval_too_brief  423)
  (:pjsip_sc_temporarily_unavailable  480)
  (:pjsip_sc_call_tsx_does_not_exist  481)
  (:pjsip_sc_loop_detected  482)
  (:pjsip_sc_too_many_hops  483)
  (:pjsip_sc_address_incomplete  484)
  (:pjsip_ac_ambiguous  485)
  (:pjsip_sc_busy_here  486)
  (:pjsip_sc_request_terminated  487)
  (:pjsip_sc_not_acceptable_here  488)
  (:pjsip_sc_bad_event  489)
  (:pjsip_sc_request_updated  490)
  (:pjsip_sc_request_pending  491)
  (:pjsip_sc_undecipherable  493)
  (:pjsip_sc_internal_server_error  500)
  (:pjsip_sc_not_implemented  501)
  (:pjsip_sc_bad_gateway  502)
  (:pjsip_sc_service_unavailable  503)
  (:pjsip_sc_server_timeout  504)
  (:pjsip_sc_version_not_supported  505)
  (:pjsip_sc_message_too_large  513)
  (:pjsip_sc_precondition_failure  580)
  (:pjsip_sc_busy_everywhere  600)
  (:pjsip_sc_decline  603)
  (:pjsip_sc_does_not_exist_anywhere  604)
  (:pjsip_sc_not_acceptable_anywhere  606)
  (:pjsip_sc_tsx_timeout 408)		 ;pjsip_sc_request_timeout
  (:pjsip_sc_tsx_transport_error  503)	 ;pjsip_sc_service_unavailable
  (:pjsip_sc__force_32bit  #x7fffffff))

(defcstruct pjsip-target
  ;;pj-list really via c macrology originally
  (prev (:pointer :void))
  (next (:pointer :void))
  (uri (:pointer (:struct pjsip-uri)))
  (q1000 :int)
  (code (:ennum pjsip-status-code)))

(defcstruct pjsip-target-set
  (head (:struct pjsip-target))
  (current (:pointer (:struct pjsip-target))))

(defcstruct pjsip-dialog
  ;;pj-list really via c macrology originally
  (prev (:pointer :void))
  (next (:pointer :void))
  (obj-name :char :count 32)
  (pool (:pointer (:struct pj-pool)))
  (mutex (:pointer :void))
  (ua (:pointer pjsip-user-agent))
  (endpt (:pointer (:struct pjsip-endpoint)))
  (dlg-set (:pointer :void))
  (state (:enum pjsip-dialog-state))
  (target (:pointer (:struct pjsip-uri)))
  (target-set (:struct pjsip-target-set))
  (inv-hdr (:struct pjsip-hdr))
  (local (:struct pjsip-dlg-party))
  (remote (:struct pjsip-dlg-party))
  (rem-cap-hdr (:struct pjsip-hdr))
  (role (:struct pjsip-role-e))
  (uac-has-2xx pj-bool))

(defcfun "pjsip_dlg_create_uac" pj-status (ua (:pointer pjsip-user-agent)) (local-uri (:pointer (:struct pj-str)))
	 (local-contact (:pointer (:struct pj-str))) (remote-uri (:pointer (:struct pj-str))) (target (:pointer (:struct pj-str)))
	 (p-dlg (:pointer (:pointer (:struct pjsip-dialog)))))

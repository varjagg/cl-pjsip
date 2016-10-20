;;;; Library wrapper

(in-package #:cl-pjsip)

(define-foreign-library libpjsip
  (:unix (:or "libpjsip.so.2" "libpjsip.so")))

(use-foreign-library libpjsip)

(defctype size :unsigned-int)
(defctype pj-status-t :int)

(defcfun "pj_init" pj-status-t)
(defcfun "pj_log_set_level" :void (log-level :int))
(defcfun "pjlib_util_init" pj-status-t)

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

(defcfun "pjsip_endpt_create" pj-status-t (factory (:pointer (:struct pj-pool-factory))) (endpt-name :string) 
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

(defcfun "pjsip_udp_transport_start" pj-status-t (endpoint (:pointer (:struct pjsip-endpoint))) (local-addr (:pointer (:struct pj-sockaddr-in)))
	 (hostport (:pointer (:struct pjsip-host-port)))
	 (async-cnt :uint) (p_transport (:pointer (:pointer (:struct pjsip-transport)))))


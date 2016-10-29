(in-package #:cl-pjsip)

(defcstruct pjmedia-clock-src
  (media-type pjmedia-type)
  (clock-rate :uint)
  (ptime-usec :uint)
  (timestamp pj-timestamp)
  (last-update pj-timestamp))

(defctype pjmedia-clock-src (:struct pjmedia-clock-src))

(defctype pjmedia-aud-dev-index :int32)

(defcenum pjmedia-aud-dev-route
  :pjmedia-aud-dev-route-default
  :pjmedia-aud-dev-route-loudspeaker
  :pjmedia-aud-dev-route-earpiece
  (:pjmedia-aud-dev-route-bluetooth 4)
  (:pjmedia-aud-dev-route-custom 128))

(defcstruct pjmedia-aud-param
  (dir pjmedia-dir)
  (rec-id pjmedia-aud-dev-index)
  (play-id pjmedia-aud-dev-index)
  (clock-rate :uint)
  (channel-count :uint)
  (samples-per-frame :uint)
  (bits-per-sample :uint)
  (flags :uint)
  (ext-fmt pjmedia-format)
  (input-latency-ms :uint)
  (output-latency-ms :uint)
  (input-vol :uint)
  (output-vol :uint)
  (input-route pjmedia-aud-dev-route)
  (output-route pjmedia-aud-dev-route)
  (ec-enabled pj-bool)
  (ec-tail-ms :uint)
  (plc-enabled pj-bool)
  (cng-enabled pj-bool)
  (vad-enabled pj-bool))

(defctype pjmedia-aud-param (:struct pjmedia-aud-param))

(defcstruct pjmedia-snd-port
  (rec-id :int)
  (play-id :int)
  (aud-caps :uint32)
  (aud-param pjmedia-aud-param)
  (aud-stream :pointer) ;dangling
  (dir pjmedia-dir)
  (port (:pointer pjmedia-port))
  (cap-clocksrc pjmedia-clock-src)
  (play-clocksrc pjmedia-clock-src)
  (clock-rate :uint)
  (channel-count :uint)
  (samples-per-frame :uint)
  (bits-per-sample :uint)
  (options :uint)
  (prm-ec-options :uint)

  (ec-state :pointer) ;dangling
  (ec-options :uint)
  (ec-tail-len :uint)
  (ec-suspended pj-bool)
  (ec-suspended-count :uint)
  (ec-suspended-limit :uint)
  ;;callbackery
  (user-data (:pointer :void))
  (on-play-frame :pointer)
  (on-rec-frame :pointer))

(defctype pjmedia-snd-port (:struct pjmedia-snd-port))

(defcfun "pjmedia_aud_subsys_init" pj-status (pf (:pointer pj-pool-factory)))

(defcfun "pjmedia_aud_subsys_shutdown" :void)

(defcfun "pjmedia_snd_port_create" :void (pool (:pointer pj-pool)) (rec_id :int) (play-id :int)
	 (clock-rate :uint) (channel-count :uint) (samples-per-frame :uint) (bits-per-sample :uint) (options :uint)
	 (p-port (:pointer (:pointer pjmedia-snd-port))))


(in-package #:cl-pjsip)

(defconstant +pjmedia-aud-default-capture-dev+ -1)
(defconstant +pjmedia-aud-default-playback-dev+ -2)
(defconstant +pjmedia-aud-invalid-dev+ +3)

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

(defcfun "pjmedia_snd_port_connect" pj-status (snd-port (:pointer pjmedia-snd-port)) (port (:pointer pjmedia-port)))

(defcfun "pjmedia_snd_port_destroy" :void (snd-port (:pointer pjmedia-snd-port)))

(defun pjmedia-pia-srate (pia)
  (let ((fmt (foreign-slot-value pia 'pjmedia-port-info 'fmt)))
    (assert (and (= (foreign-slot-value fmt 'pjmedia-format 'type) (foreign-enum-value 'pjmedia-type :pjmedia-type-audio))
		 (= (foreign-slot-value fmt 'pjmedia-format 'detail-type)
		    (foreign-enum-value 'pjmedia-format-detail-type :pjmedia-format-detail-audio))))
    (foreign-slot-value (foreign-slot-pointer (foreign-slot-pointer fmt 'pjmedia-format 'det)
						  '(:union format-det) 'aud)
			    'pjmedia-audio-format-detail 'clock-rate)))

(defun pjmedia-pia-ccnt (pia)
  (let ((fmt (foreign-slot-value pia 'pjmedia-port-info 'fmt)))
    (assert (and (= (foreign-slot-value fmt 'pjmedia-format 'type) (foreign-enum-value 'pjmedia-type :pjmedia-type-audio))
		 (= (foreign-slot-value fmt 'pjmedia-format 'detail-type)
		    (foreign-enum-value 'pjmedia-format-detail-type :pjmedia-format-detail-audio))))
    (foreign-slot-value (foreign-slot-pointer (foreign-slot-pointer fmt 'pjmedia-format 'det)
						  '(:union format-det) 'aud)
			'pjmedia-audio-format-detail 'channel-count)))

(defun pjmedia-pia-bits (pia)
  (let ((fmt (foreign-slot-value pia 'pjmedia-port-info 'fmt)))
    (assert (and (= (foreign-slot-value fmt 'pjmedia-format 'type) (foreign-enum-value 'pjmedia-type :pjmedia-type-audio))
		 (= (foreign-slot-value fmt 'pjmedia-format 'detail-type)
		    (foreign-enum-value 'pjmedia-format-detail-type :pjmedia-format-detail-audio))))
    (foreign-slot-value (foreign-slot-pointer (foreign-slot-pointer fmt 'pjmedia-format 'det)
						  '(:union format-det) 'aud)
			    'pjmedia-audio-format-detail 'bits-per-sample)))

(defun pjmedia-spf (clock-rate usec-ptime channel-count)
  (/ (* usec-ptime clock-rate channel-count) 1000000))

(defun pjmedia-afd-spf (pafd)
  (pjmedia-spf (foreign-slot-value pafd 'pjmedia-audio-format-detail 'clock-rate)
	       (foreign-slot-value pafd 'pjmedia-audio-format-detail 'frame-time-usec)
	       (foreign-slot-value pafd 'pjmedia-audio-format-detail 'channel-count)))

(defun pjmedia-pia-spf (pia)
-  (let ((fmt (foreign-slot-value pia 'pjmedia-port-info 'fmt)))
    (assert (and (= (foreign-slot-value fmt 'pjmedia-format 'type) (foreign-enum-value 'pjmedia-type :pjmedia-type-audio))
		 (= (foreign-slot-value fmt 'pjmedia-format 'detail-type)
		    (foreign-enum-value 'pjmedia-format-detail-type :pjmedia-format-detail-audio))))
    (pjmedia-afd-spf (foreign-slot-pointer (foreign-slot-pointer fmt 'pjmedia-format 'det)
						  '(:union format-det) 'aud))))

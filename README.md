# CL-PJSIP

Common Lisp wrapper for [PJSIP](http://www.pjsip.org/) multimedia communication library.

Work in progress. Current goals:

* Create FFI bindings for practically viable subset of PJSIP [done]
* Create a simple SIP user agent capable of handling basic sessions and establishing media streams [done]
* Implement more ideomatic Lisp application logic concepts
* Support PJSUA high level softphone interface [done]
* Add various media ports (audio [done], file, null..)

Still in alpha quality. Expect glitches and crashes.

Licensed under the terms of GPL v2 in compliance with PJSIP code it links to.

## Dependencies

The demo application is known to work on Linux x86_64 with CCL 1.11 and Allegro CL 10.1beta. SBCL 1.3.5.9 does experience sporadic crashes. [PJSIP](http://www.pjsip.org/) 2.5.5 is recommended.

## Building PJSIP

```
./configure --enable-shared
make dep
make
make install
```

You might want to run `ldconfig` after the install to update the libary cache.


## Running CL-PJSUA demo

Load the :cl-pjsua-demo system, then 
```
(cl-pjsip::run-pjsua "your destination sip address")
```

Alternatively, you can run "plain" PJSIP agent in echo-server mode via
```
(cl-pjsip:init)
(cl-pjsip:run-agent)
```

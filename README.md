# CL-PJSIP

Common Lisp wrapper for [PJSIP](http://www.pjsip.org/) multimedia communication library.

Work in progress. Current goals:

* Create FFI bindings for practically viable subset of PJSIP [done]
* Create a simple SIP user agent capable of handling basic sessions and establishing media streams [done]
* Implement more ideomatic Lisp application logic concepts
* Add various media ports (audio [done], file, null..)

Still in alpha quality. Expect glitches and crashes.

Licensed under the terms of GPL v2 in compliance with PJSIP code it links to.

## Dependencies

Known to work on CCL 1.11 x64, with [PJSIP](http://www.pjsip.org/) 2.5.5.

## Building PJSIP

```
./configure --enable-shared
make dep
make
make install
```

You might want to run `ldconfig` after the install to update the libary cache.


## Running CL-PJSIP

Load the :cl-pjsip-ua system, then 
```
(cl-pjsip:init)
(cl-pjsip:run-agent "your destination sip address")
```

..alternatively, run the agent in sever mode by omitting the destination string.

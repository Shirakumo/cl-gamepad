(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.gamepad
  (:use #:cl)
  (:intern
   #:button-map
   #:axis-map
   #:button-states
   #:axis-states
   #:axis-raw-states
   #:axis-ramps
   #:axis-dead-zones
   #:label-id
   #:id-label)
  ;; configurator.lisp
  (:export
   #:configure-device
   #:monitor-device)
  ;; mapping.lisp
  (:export
   #:*default-mappings-file*
   #:blacklisted-p
   #:device-mapping
   #:remove-device-mapping
   #:define-device-mapping
   #:save-device-mappings)
  ;; protocol.lisp
  (:export
   #:+labels+
   #:+label-descriptions+
   #:+icon-types+
   #:gamepad-error
   #:event
   #:event-device
   #:event-time
   #:event-code
   #:event-label
   #:event-value
   #:event-old-value
   #:button-down
   #:button-up
   #:axis-move
   #:device
   #:name
   #:vendor
   #:product
   #:version
   #:driver
   #:icon-type
   #:button
   #:axis
   #:init
   #:shutdown
   #:list-devices
   #:call-with-devices
   #:poll-devices
   #:poll-events
   #:drop-device
   #:rumble
   #:dead-zone
   #:ramp
   #:ensure-device))

(defpackage #:org.shirakumo.fraf.gamepad.impl
  (:use #:cl)
  (:shadow #:byte)
  (:import-from
   #:org.shirakumo.fraf.gamepad
   #:+labels+
   #:button-map
   #:axis-map
   #:button-states
   #:axis-states
   #:axis-raw-states
   #:axis-ramps
   #:axis-dead-zones
   #:label-id
   #:id-label
   #:init
   #:shutdown
   #:list-devices
   #:call-with-devices
   #:poll-devices
   #:poll-events
   #:drop-device
   #:rumble
   #:blacklisted-p)
  (:local-nicknames
   (#:gamepad #:org.shirakumo.fraf.gamepad)
   #+windows (#:com #:org.shirakumo.com-on)
   #+windows (#:com-cffi #:org.shirakumo.com-on.cffi)))

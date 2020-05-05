#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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
   #:configure-device)
  ;; mapping.lisp
  (:export
   #:*default-mappings-file*
   #:device-mapping
   #:remove-device-mapping
   #:define-device-mapping
   #:save-device-mappings)
  ;; protocol.lisp
  (:export
   #:+labels+
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
   #:button
   #:axis
   #:init
   #:shutdown
   #:list-devices
   #:poll-devices
   #:poll-events
   #:drop-device
   #:rumble
   #:dead-zone
   #:ramp))

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
   #:poll-devices
   #:poll-events
   #:drop-device
   #:rumble)
  (:local-nicknames
   (#:gamepad #:org.shirakumo.fraf.gamepad)
   #+win32 (#:com #:org.shirakumo.com-on)))

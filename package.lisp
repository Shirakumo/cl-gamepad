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
   #:axis-map)
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
   #:button-down
   #:button-up
   #:axis-move
   #:device
   #:name
   #:vendor
   #:product
   #:version
   #:driver
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
   #:init
   #:shutdown
   #:list-devices
   #:poll-devices
   #:poll-events
   #:drop-device
   #:rumble)
  (:local-nicknames
   (#:gamepad #:org.shirakumo.fraf.gamepad)))

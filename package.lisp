#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.gamepad
  (:use #:cl)
  (:export
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
   #:driver-version
   #:label-id
   #:id-label
   #:init
   #:shutdown
   #:list-devices
   #:poll-devices
   #:poll-events
   #:rumble))

(defpackage #:org.shirakumo.fraf.gamepad.impl
  (:use #:cl)
  (:shadow #:byte)
  (:import-from
   (#:gamepad
    #:+labels+
    #:signal-button-down
    #:signal-button-up
    #:signal-axis-move
    #:button-map
    #:axis-map))
  (:local-nicknames
   (#:gamepad #:org.shirakumo.fraf.gamepad)))

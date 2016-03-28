#|
 This file is a part of cl-gamepad
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:cl-gamepad-cffi
  (:nicknames #:org.shirakumo.fraf.gamepad.cffi)
  (:use #:cl #:cffi)
  ;; low-level.lisp
  (:export
   #:libstem-gamepad
   #:device
   #:device-id
   #:device-description
   #:device-vendor
   #:device-product
   #:device-axis-count
   #:device-button-count
   #:device-axis-states
   #:device-button-states
   #:device-private-data
   #:device-attach-func
   #:device-remove-func
   #:button-down-func
   #:button-up-func
   #:axis-move-func
   #:gamepad-init
   #:gamepad-shutdown
   #:gamepad-num-devices
   #:gamepad-device-at-index
   #:gamepad-detect-devices
   #:gamepad-process-events
   #:gamepad-device-attach-func
   #:gamepad-device-remove-func
   #:gamepad-button-down-func
   #:gamepad-button-up-func
   #:gamepad-axis-move-func))

(defpackage #:cl-gamepad
  (:nicknames #:org.shirakumo.fraf.gamepad)
  (:use #:cl #:org.shirakumo.fraf.gamepad.cffi)
  ;; wrapper.lisp
  (:export
   #:device-attached
   #:device-removed
   #:button-pressed
   #:button-released
   #:axis-moved
   #:init
   #:shutdown
   #:device-count
   #:device
   #:devices
   #:detect-devices
   #:process-events))

#|
 This file is a part of cl-gamepad
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:cl-gamepad
  (:nicknames #:org.shirakumo.fraf.gamepad)
  (:use #:cl :cffi)
  ;; low-level.lisp
  (:export
   #:device-id
   #:device-description
   #:device-vendor
   #:device-product
   #:device-axes
   #:device-buttons
   #:device-axis-states
   #:device-button-states
   #:device-private-data
   #:device-axis
   #:device-button)
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
   #:detect-devices
   #:process-events))

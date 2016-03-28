#|
 This file is a part of cl-gamepad
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad)

(define-foreign-library libstem-gamepad
  (:darwin (:or "libstem_gamepad.dylib" "libstem_gamepad.so"))
  (:unix (:or "libstem_gamepad.so"))
  (t (:default "libstem_gamepad")))

(use-foreign-library libstem-gamepad)

(defcstruct gamepad-device
  (device-id :uint)
  (description :string)
  (vendor :int)
  (product :int)
  (axes :int)
  (buttons :int)
  (axis-states (:pointer :float))
  (button-states (:pointer :bool))
  (private-data :pointer))

(defcallback device-attach-func :void ((device :pointer (:struct gamepad-device)) (context :pointer))
  (ignore-errors (device-attached device)))

(defcallback device-remove-func :void ((device :pointer (:struct gamepad-device)) (context :pointer))
  (ignore-errors (device-removed device)))

(defcallback button-down-func :void ((device :pointer (:struct gamepad-device)) (button :uint) (timestamp :double) (context :pointer))
  (ignore-errors (button-pressed button time device)))

(defcallback button-up-func :void ((device :pointer (:struct gamepad-device)) (button :uint) (timestamp :double) (context :pointer))
  (ignore-errors (button-released button time device)))

(defcallback axis-move-func :void ((device :pointer (:struct gamepad-device)) (axis :uint) (value :float) (last-value :float) (timestamp :double) (context :pointer))
  (ignore-errors (axis-moved axis last-value value time device)))

(defcfun (gamepad-init "Gamepad_init") :void)

(defcfun (gamepad-shutdown "Gamepad_shutdown") :void)

(defcfun (gamepad-num-devices "Gamepad_numDevices") :uint)

(defcfun (gamepad-device-at "Gamepad_deviceAtIndex") (:pointer (:struct gamepad-device))
  (device-index :uint))

(defcfun (gamepad-detect-devices "Gamepad_detectDevices") :void)

(defcfun (gamepad-process-events "Gamepad_processEvents") :void)

(defcfun (gamepad-device-attach-func "Gamepad_deviceAttachFunc") :void
  (callback :pointer)
  (context :pointer))

(defcfun (gamepad-device-remove-func "Gamepad_deviceRemoveFunc") :void
  (callback :pointer)
  (context :pointer))

(defcfun (gamepad-button-down-func "Gamepad_buttonDownFunc") :void
  (callback :pointer)
  (context :pointer))

(defcfun (gamepad-button-up-func "Gamepad_buttonUpFunc") :void
  (callback :pointer)
  (context :pointer))

(defcfun (gamepad-axis-move-func "Gamepad_axisMoveFunc") :void
  (callback :pointer)
  (context :pointer))

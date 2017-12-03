#|
 This file is a part of cl-gamepad
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

(define-foreign-library vcruntime140
  (:windows (:or "vcruntime140.dll"
                 "vcruntime140d.dll")))

(define-foreign-library libstem-gamepad
  (:darwin (:or "libstem_gamepad.dylib" "libstem_gamepad.so"
                #+X86 "mac32-libstem_gamepad.dylib"
                #+X86-64 "mac64-libstem_gamepad.dylib"))
  (:unix (:or "libstem_gamepad.so"
              #+X86 "lin32-libstem_gamepad.so"
              #+X86-64 "lin64-libstem_gamepad.so"))
  (:windows (:or "stem_gamepad.dll"
                 #+X86 "win32-libstem_gamepad.dll"
                 #+X86-64 "win64-libstem_gamepad.dll"))
  (t (:default "stem_gamepad")))

(use-foreign-library vcruntime140)
(use-foreign-library libstem-gamepad)

(defcenum button
  :unknown
  :x
  :y
  :z
  :a
  :b
  :c
  :l1
  :l2
  :r1
  :r2
  :l
  :r
  :dpad-l
  :dpad-r
  :dpad-u
  :dpad-d
  :select
  :home
  :start
  :trigger)

(defcenum axis
  :unknown
  :l-h
  :l-v
  :r-h
  :r-v
  :dpad-h
  :dpad-v
  :dpad-l
  :dpad-r
  :dpad-u
  :dpad-d
  :button-x
  :button-y
  :button-z
  :button-a
  :button-b
  :button-c
  :l1
  :l2
  :r1
  :r2
  :tilt-x
  :tilt-y
  :tilt-z
  :move-x
  :move-y
  :move-z
  :wheel
  :accelerator
  :brake
  :x
  :y
  :z
  :throttle)

(defcstruct (device :class device :conc-name device-)
  (id :uint)
  (description :string)
  (vendor :int)
  (product :int)
  (axis-count :uint)
  (button-count :uint)
  (axis-states (:pointer :float))
  (button-states (:pointer :uint))
  (device-map :pointer)
  (private-data :pointer))

(defcstruct (device-map :class device-map :conc-name device-map)
  (button-map button :count 32)
  (axis-map axis :count 32)
  (axis-multipliers :char :count 32))

(declaim (inline device-map-buttons device-map-axes device-map-axis-multipliers))
(defun device-map-buttons (device-map)
  (cffi:foreign-slot-pointer device-map '(:struct device-map) 'button-map))

(defun device-map-axes (device-map)
  (cffi:foreign-slot-pointer device-map '(:struct device-map) 'axis-map))

(defun device-map-axis-multipliers (device-map)
  (cffi:foreign-slot-pointer device-map '(:struct device-map) 'axis-multipliers))

(defmacro with-callback-handling (() &body body)
  `(ignore-errors
    (with-simple-restart (continue "Ignore problem and continue.")
      (handler-bind ((error #'invoke-debugger))
        ,@body))))

(defun device-attached (device) (declare (ignore device)))
(defun device-removed (device) (declare (ignore device)))
(defun button-pressed (button time device) (declare (ignore button time device)))
(defun button-released (button time device) (declare (ignore button time device)))
(defun axis-moved (axis last-value value time device) (declare (ignore axis last-value value time device)))

(defcallback device-attach-func :void ((device :pointer (:struct device)) (context :pointer))
  (declare (ignore context))
  (with-callback-handling ()
    (device-attached device)))

(defcallback device-remove-func :void ((device :pointer (:struct device)) (context :pointer))
  (declare (ignore context))
  (with-callback-handling ()
    (device-removed device)))

(defcallback button-down-func :void ((device :pointer (:struct device)) (button :uint) (timestamp :double) (context :pointer))
  (declare (ignore context))
  (with-callback-handling ()
    (button-pressed button timestamp device)))

(defcallback button-up-func :void ((device :pointer (:struct device)) (button :uint) (timestamp :double) (context :pointer))
  (declare (ignore context))
  (with-callback-handling ()
    (button-released button timestamp device)))

(defcallback axis-move-func :void ((device :pointer (:struct device)) (axis :uint) (value :float) (last-value :float) (timestamp :double) (context :pointer))
  (declare (ignore context))
  (with-callback-handling ()
    (axis-moved axis last-value value timestamp device)))

(defcfun (gamepad-init "Gamepad_init") :void)

(defcfun (gamepad-shutdown "Gamepad_shutdown") :void)

(defcfun (gamepad-num-devices "Gamepad_numDevices") :uint)

(defcfun (gamepad-device-at-index "Gamepad_deviceAtIndex") (:pointer (:struct device))
  (device-index :uint))

(defcfun (gamepad-device-map "Gamepad_deviceMap") (:pointer (:struct device-map))
  (vendor-id :int)
  (product-id :int))

(defcfun (gamepad-set-device-map "Gamepad_setDeviceMap") :char
  (vendor-id :int)
  (product-id :int)
  (map :pointer))

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

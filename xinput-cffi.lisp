#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.impl)

(cffi:define-foreign-library xinput
  (T (:or "XInput9_1_0.dll" "XInput1_4.dll" "XInput1_3.dll")))

(cffi:defctype dword :uint32)
(cffi:defctype word :uint16)
(cffi:defctype short :int16)
(cffi:defctype byte :uint8)

(cffi:defbitfield buttons
  (:dpad-u #x0001)
  (:dpad-d #x0002)
  (:dpad-l #x0004)
  (:dpad-r #x0008)
  (:start  #x0010)
  (:back   #x0020)
  (:l3     #x0040)
  (:r3     #x0080)
  (:l1     #x0100)
  (:r1     #x0200)
  (:a      #x1000)
  (:b      #x2000)
  (:x      #x4000)
  (:y      #x8000))

(cffi:defcenum (errno dword)
  (:success              #x0000)
  (:device-not-connected #x048F)
  (:empty                #x10D2))

(cffi:defcstruct (gamepad :conc-name gamepad-)
  (buttons buttons)
  (left-trigger byte)
  (right-trigger byte)
  (thumb-lx short)
  (thumb-ly short)
  (thumb-rx short)
  (thumb-ry short))

(cffi:defcstruct (vibration :conc-name vibration)
  (left word)
  (right word))

(cffi:defcstruct (capabilities :conc-name capabilities-)
  (type byte)
  (subtype byte)
  (flags word)
  (gamepad (:struct gamepad))
  (vibration (:struct vibration)))

(cffi:defcfun (get-capabilities "XInputGetCapabilities") errno
  (user-index dword)
  (flags dword)
  (capabilities :pointer))

(cffi:defcfun (get-state "XInputGetState") errno
  (user-index dword)
  (state :pointer))

(cffi:defcfun (set-state "XInputSetState") errno
  (user-index dword)
  (vibration :pointer))

(cffi:defcfun (get-keystroke "XInputGetKeystroke") errno
  (user-index dword)
  (reserved dword)
  (keystroke :pointer))





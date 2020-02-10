#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.impl)

(cffi:define-foreign-library xinput
  (T (:or "XInput9_1_0.dll" "XInput1_4.dll" "XInput1_3.dll")))

(cffi:defbitfield (xbuttons dword)
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
  (:ok                   #x0000)
  (:device-not-connected #x048F)
  (:empty                #x10D2))

(cffi:defcstruct (xgamepad :conc-name xgamepad-)
  (buttons dword)
  (left-trigger byte)
  (right-trigger byte)
  (lx short)
  (ly short)
  (rx short)
  (ry short))

(cffi:defcstruct (xstate :conc-name xstate-)
  (packet dword)
  (gamepad (:struct xgamepad)))

(cffi:defcstruct (xvibration :conc-name xvibration)
  (left word)
  (right word))

(cffi:defcstruct (xcapabilities :conc-name xcapabilities-)
  (type byte)
  (subtype byte)
  (flags word)
  (gamepad (:struct xstate))
  (vibration (:struct xvibration)))

(cffi:defcfun (get-xcapabilities "XInputGetCapabilities") errno
  (user-index dword)
  (flags dword)
  (capabilities :pointer))

(cffi:defcfun (get-xstate "XInputGetState") errno
  (user-index dword)
  (state :pointer))

(cffi:defcfun (set-xstate "XInputSetState") errno
  (user-index dword)
  (vibration :pointer))

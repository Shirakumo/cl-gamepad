#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.impl)

(cffi:define-foreign-library user32
  (T (:default "User32")))

(defconstant MAX-PATH 260)
(defvar HWND-MESSAGE (cffi:make-pointer (- (ash 1 #+64-bit 64 #-64-bit 32) 3)))

(cffi:defctype dword :uint32)
(cffi:defctype word :uint16)
(cffi:defctype long :int32)
(cffi:defctype short :int16)
(cffi:defctype byte :uint8)
(cffi:defctype wchar :uint16)
(cffi:defctype uint-ptr #+64-bit :uint64 #-64-bit :uint32)

(cffi:defcenum (hresult :int :allow-undeclared-values T)
  (:ok #x00000000)
  (:false #x00000001)
  (:polled-device #x00000002)
  (:abort #x80004004)
  (:cancelled #x800704C7)
  (:access-denied #x80070005)
  (:fail #x80004005)
  (:handle #x80070006)
  (:invalid-arg #x80070057)
  (:no-interface #x80004002)
  (:not-implemented #x80004001)
  (:out-of-memory #x8007000e)
  (:pointer #x80004003)
  (:unexpected #x8000ffff)
  (:input-lost #x8007001e)
  (:not-acquired #x8007000c)
  (:not-initialized #x80070015)
  (:other-has-priority #x80070005)
  (:invalid-parameter #x80070057)
  (:not-buffered #x80040207)
  (:acquired #x800700aa)
  (:handle-exists #x80070005)
  (:unplugged #x80040209)
  (:device-full #x80040201)
  (:device-not-reg #x80040154)
  ;; KLUDGE: for Xinput in win32-error
  (:not-connected #x048F))

(cffi:defcenum (wait-result dword)
  (:ok #x00)
  (:abandoned #x80)
  (:io-completion #xC0)
  (:timeout #x102)
  (:failed #xFFFFFFFF))

(cffi:defcenum (hid-device-type dword)
  (:mouse 0)
  (:keyboard 1)
  (:hid 2))

(cffi:defcenum (hid-device-info-command :uint)
  (:device-name    #x20000007)
  (:device-info    #x2000000b)
  (:preparsed-data #x20000005))

(cffi:defcenum (win-device-type dword)
  (:oem              #x00000000)
  (:device-node      #x00000001)
  (:volume           #x00000002)
  (:port             #x00000003)
  (:net              #x00000004)
  (:device-interface #x00000005)
  (:handle           #x00000006))

(cffi:defcenum (wparam #+64-bit :uint64 #-64-bit :unsigned-long :allow-undeclared-values T)
  (:no-disk-space              #x0047)
  (:low-disk-space             #x0048)
  (:config-message-private     #x7fff)
  (:device-arrival             #x8000)
  (:device-query-remove        #x8001)
  (:device-query-remove-failed #x8002)
  (:device-remove-pending      #x8003)
  (:device-remove-complete     #x8004)
  (:device-type-specific       #x8005)
  (:custom-event               #x8006))

(cffi:defcenum (window-message :uint :allow-undeclared-values T)
  (:device-change #x0219))

(cffi:defcstruct (window-class :conc-name window-class-)
  (size :uint)
  (style :uint)
  (procedure :pointer)
  (class-extra :int)
  (window-extra :int)
  (instance :pointer)
  (icon :pointer)
  (cursor :pointer)
  (background :pointer)
  (menu-name :pointer)
  (class-name :pointer)
  (small-icon :pointer))

(cffi:defcstruct (point :conc-name point-)
  (x long)
  (y long))

(cffi:defcstruct (message :conc-name message-)
  (window :pointer)
  (message window-message)
  (wparam wparam)
  (lparam :pointer)
  (time dword)
  (point (:struct point))
  (private dword))

(cffi:defcstruct (raw-input-device-list :conc-name raw-input-device-list-)
  (device :pointer)
  (type hid-device-type))

(cffi:defcstruct (hid-device-info :conc-name hid-device-info-)
  (size dword)
  (type hid-device-type)
  (vendor-id dword)
  (product-id dword)
  (version-number dword)
  (usage-page :ushort)
  (usage :ushort)
  (pad :uint64))

(cffi:defcfun (get-module-handle "GetModuleHandleW") :pointer
  (module-name :pointer))

(cffi:defcfun (get-active-window "GetActiveWindow") :pointer)

(cffi:defcfun (register-class "RegisterClassExW") word
  (class :pointer))

(cffi:defcfun (unregister-class "UnregisterClassW") :void
  (class-name :pointer)
  (handle :pointer))

(cffi:defcfun (create-window "CreateWindowExW") :pointer
  (ex-style dword)
  (class-name :pointer)
  (window-name :pointer)
  (style dword)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (parent :pointer)
  (menu :pointer)
  (instance :pointer)
  (param :pointer))

(cffi:defcfun (destroy-window "DestroyWindow") :void
  (window :pointer))

(cffi:defcfun (register-device-notification "RegisterDeviceNotificationW") :pointer
  (recipient :pointer)
  (filter :pointer)
  (flags dword))

(cffi:defcfun (unregister-device-notification "UnregisterDeviceNotification") :void
  (notification :pointer))

(cffi:defcfun (default-window-handler "DefWindowProcW") :pointer
  (window :pointer)
  (message window-message)
  (wparam wparam)
  (lparam :pointer))

(cffi:defcfun (wait-for-single-object "WaitForSingleObject") wait-result
  (handle :pointer)
  (milliseconds dword))

(cffi:defcfun (peek-message "PeekMessageW") :bool
  (message :pointer)
  (window :pointer)
  (filter-min :uint)
  (filter-max :uint)
  (remove-msg :uint))

(cffi:defcfun (get-message "GetMessageW") :bool
  (message :pointer)
  (window :pointer)
  (filter-min :uint)
  (filter-max :uint))

(cffi:defcfun (translate-message "TranslateMessage") :bool
  (message :pointer))

(cffi:defcfun (dispatch-message "DispatchMessageW") :pointer
  (message :pointer))

(cffi:defcfun (create-event "CreateEventW") :pointer
  (attributes :pointer)
  (manual-reset :bool)
  (initial-state :bool)
  (name :pointer))

(cffi:defcfun (close-handle "CloseHandle") :void
  (handle :pointer))

(cffi:defcfun (set-timer "SetTimer") uint-ptr
  (window :pointer)
  (event uint-ptr)
  (elapse :uint)
  (func :pointer))

(cffi:defcfun (kill-timer "KillTimer") :bool
  (window :pointer)
  (event uint-ptr))

(cffi:defcfun (memcmp "memcmp") :int
  (a :pointer)
  (b :pointer)
  (n :uint))

(cffi:defcfun (memset "memset") :pointer
  (pointer :pointer)
  (fill :int)
  (n :uint))

(cffi:defcfun (get-raw-input-device-list "GetRawInputDeviceList") :uint
  (list :pointer)
  (num :pointer)
  (size :uint))

(cffi:defcfun (get-raw-input-device-info "GetRawInputDeviceInfoW") :uint
  (device :pointer)
  (command hid-device-info-command)
  (data :pointer)
  (size :pointer))

(com:define-guid GUID-DEVINTERFACE-HID #x4D1E55B2 #xF16F #x11CF #x88 #xCB #x00 #x11 #x11 #x00 #x00 #x30)

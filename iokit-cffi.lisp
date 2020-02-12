#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.impl)

(cffi:define-foreign-library iokit
  (T (:framework "IOKit")))

(defvar DEVICE-USAGE-PAGE-KEY "DeviceUsagePage")
(defvar DEVICE-USAGE-KEY "DeviceUsage")
(defvar PRODUCT-KEY "Product")
(defvar PRODUCT-ID-KEY "ProductID")
(defvar VENDOR-ID-KEY "VendorID")
(defvar VERSION-NUMBER-KEY "VersionNumber")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun kio-err (x)
    (flet ((err-system (x)
             (ash (logand x #x3f) 26))
           (err-sub (x)
             (ash (logand x #xFFF) 14)))
      (logior (err-system #x38) (err-sub 0) x))))

(cffi:defcenum io-return
  (:error             #.(kio-err 700)) ; general error    
  (:no-memory         #.(kio-err 701)) ; can't allocate memory 
  (:no-resources      #.(kio-err 702)) ; resource shortage 
  (:ipc-error         #.(kio-err 703)) ; error during IPC 
  (:no-device         #.(kio-err 704)) ; no such device 
  (:not-privileged    #.(kio-err 705)) ; privilege violation 
  (:bad-argument      #.(kio-err 706)) ; invalid argument 
  (:locked-read       #.(kio-err 707)) ; device read locked 
  (:locked-write      #.(kio-err 708)) ; device write locked 
  (:exclusive-access  #.(kio-err 709)) ; exclusive access and
  (:bad-message-id    #.(kio-err 710)) ; sent/received messages
  (:unsupported       #.(kio-err 711)) ; unsupported function 
  (:vm-error          #.(kio-err 712)) ; misc. VM failure 
  (:internal-error    #.(kio-err 713)) ; internal error 
  (:io-error          #.(kio-err 714)) ; General I/O error 
  (:cannot-lock       #.(kio-err 716)) ; can't acquire lock
  (:not-open          #.(kio-err 717)) ; device not open 
  (:not-readable      #.(kio-err 718)) ; read not supported 
  (:not-writable      #.(kio-err 719)) ; write not supported 
  (:not-aligned       #.(kio-err 720)) ; alignment error 
  (:bad-media         #.(kio-err 721)) ; Media Error 
  (:still-open        #.(kio-err 722)) ; device(s) still open 
  (:rld-error         #.(kio-err 723)) ; rld failure 
  (:dma-error         #.(kio-err 724)) ; DMA failure 
  (:busy              #.(kio-err 725)) ; Device Busy 
  (:timeout           #.(kio-err 726)) ; I/O Timeout 
  (:offline           #.(kio-err 727)) ; device offline 
  (:not-ready         #.(kio-err 728)) ; not ready 
  (:not-attached      #.(kio-err 729)) ; device not attached 
  (:no-channels       #.(kio-err 730)) ; no DMA channels left
  (:no-space          #.(kio-err 731)) ; no space for data 
  (:port-exists       #.(kio-err 733)) ; port already exists
  (:cannot-wire       #.(kio-err 734)) ; can't wire down 
  (:no-interrupt      #.(kio-err 735)) ; no interrupt attached
  (:no-frames         #.(kio-err 736)) ; no DMA frames enqueued
  (:message-too-large #.(kio-err 737)) ; oversized msg received
  (:not-permitted     #.(kio-err 738)) ; not permitted
  (:no-power          #.(kio-err 739)) ; no power to device
  (:no-media          #.(kio-err 740)) ; media not present
  (:unformatted-media #.(kio-err 741)) ; media not formatted
  (:unsupported-mode  #.(kio-err 742)) ; no such mode
  (:underrun          #.(kio-err 743)) ; data underrun
  (:overrun           #.(kio-err 744)) ; data overrun
  (:device-error      #.(kio-err 745)) ; The device is not working properly!
  (:no-completion     #.(kio-err 746)) ; A completion routine is required
  (:aborted           #.(kio-err 747)) ; Operation aborted
  (:no-bandwidth      #.(kio-err 748)) ; Bus bandwidth would be exceeded
  (:not-responding    #.(kio-err 749)) ; Device not responding
  (:iso-too-old       #.(kio-err 750)) ; Isochronous I/O request for distant past!
  (:iso-too-new       #.(kio-err 751)) ; Isochronous I/O request for distant future
  (:invalid           #.(kio-err   1)) ; should never be seen
  (:success                         0))

(cffi:defcenum (io-page :uint)
  (:undefined               #x00)
  (:generic-desktop         #x01)
  (:simulation              #x02)
  (:vr                      #x03)
  (:sport                   #x04)
  (:game                    #x05)
  (:generic-device-controls #x06)
  (:keyboard-or-keypad      #x07)
  (:leds                    #x08)
  (:button                  #x09)
  (:ordinal                 #x0a)
  (:telephony               #x0b)
  (:consumer                #x0c)
  (:digitizer               #x0d)
  (:pid                     #x0f)
  (:unicode                 #x10)
  (:alphanumeric-display    #x14)
  (:sensor                  #x20)
  (:monitor                 #x80)
  (:monitor-enumerated      #x81)
  (:monitor-virtual         #x82)
  (:monitor-reserved        #x83)
  (:power-device            #x84)
  (:battery-system          #x85)
  (:power-reserved          #x86)
  (:power-reserved2         #x87)
  (:barcode-scanner         #x8c)
  (:weighing-device         #x8d)
  (:scale                   #x8d)
  (:magnetic-strip-ereader  #x8e)
  (:camera-control          #x90)
  (:arcade                  #x91)
  (:vendor-defined-start    #xff00))

(cffi:defcenum (io-desktop-usage :uint)
  (:undefined #x00)
  (:pointer #x01)   ; Physical Collection
  (:mouse #x02)   ; Application Collection
  (:joystick #x04)   ; Application Collection
  (:gamepad #x05)   ; Application Collection
  (:keyboard #x06)   ; Application Collection
  (:keypad #x07)   ; Application Collection
  (:multi-axis-controller #x08)   ; Application Collection
  (:x #x30)   ; Dynamic Value
  (:y #x31)   ; Dynamic Value
  (:z #x32)   ; Dynamic Value
  (:rx #x33)   ; Dynamic Value
  (:ry #x34)   ; Dynamic Value
  (:rz #x35)   ; Dynamic Value
  (:slider #x36)   ; Dynamic Value
  (:dial #x37)   ; Dynamic Value
  (:wheel #x38)   ; Dynamic Value
  (:hat-switch #x39)   ; Dynamic Value
  (:counted-buffer #x3A)   ; Logical Collection
  (:byte-count #x3B)   ; Dynamic Value
  (:motion-wake-up #x3C)   ; One-Shot Control
  (:start #x3D)   ; On/Off Control
  (:select #x3E)   ; On/Off Control
  (:vx #x40)   ; Dynamic Value
  (:vy #x41)   ; Dynamic Value
  (:vz #x42)   ; Dynamic Value
  (:vbrx #x43)   ; Dynamic Value
  (:vbry #x44)   ; Dynamic Value
  (:vbrz #x45)   ; Dynamic Value
  (:vno #x46)   ; Dynamic Value
  (:system-control #x80)   ; Application Collection
  (:system-powerdown #x81)   ; One-Shot Control
  (:system-sleep #x82)   ; One-Shot Control
  (:system-wakeup #x83)   ; One-Shot Control
  (:system-context-menu #x84)   ; One-Shot Control
  (:system-main-menu #x85)   ; One-Shot Control
  (:system-app-menu #x86)   ; One-Shot Control
  (:system-menu-help #x87)   ; One-Shot Control
  (:system-menu-exit #x88)   ; One-Shot Control
  (:system-menu-select #x89)   ; Selector
  (:system-menu #x89)   ; Selector
  (:system-menu-right #x8A)   ; Re-Trigger Control
  (:system-menu-left #x8B)   ; Re-Trigger Control
  (:system-menu-up #x8C)   ; Re-Trigger Control
  (:system-menu-down #x8D)   ; Re-Trigger Control
  (:dpad-up #x90)   ; On/Off Control
  (:dpad-down #x91)   ; On/Off Control
  (:dpad-right #x92)   ; On/Off Control
  (:dpad-left #x93)   ; On/Off Control
  (:reserved #xFFFF))

(cffi:defcenum (hid-element-type)
  (:input-misc          1)
  (:input-button        2)
  (:input-axis          3)
  (:input-scan-codes    4)
  (:output            129)
  (:feature           257)
  (:collection        513))

(cffi:defcfun (create-hid-manager "IOHIDManagerCreate") :pointer
  (allocator :pointer)
  (options :uint32))

(cffi:defcfun (set-matching-multiple "IOHIDManagerSetDeviceMatchingMultiple") :void
  (manager :pointer)
  (multiple :pointer))

(cffi:defcfun (register-device-match-callback "IOHIDManagerRegisterDeviceMatchingCallback") :void
  (manager :pointer)
  (callback :pointer)
  (user :pointer))

(cffi:defcfun (register-device-remove-callback "IOHIDManagerRegisterDeviceRemovalCallback") :void
  (manager :pointer)
  (callback :pointer)
  (user :pointer))

(cffi:defcfun (open-manager "IOHIDManagerOpen") io-return
  (manager :pointer)
  (options :uint32))

(cffi:defcfun (close-manager "IOHIDManagerClose") io-return
  (manager :pointer)
  (options :uint32))

(cffi:defcfun (manager-schedule-with-run-loop "IOHIDManagerScheduleWithRunLoop") :void
  (manager :pointer)
  (run-loop :pointer)
  (mode :pointer))

(cffi:defcfun (manager-unschedule-from-run-loop "IOHIDManagerUnscheduleFromRunLoop") :void
  (manager :pointer)
  (run-loop :pointer)
  (mode :pointer))

(cffi:defcfun (device-int-property "IOHIDDeviceGetIntProperty") :long
  (device :pointer)
  (key :pointer))

(cffi:defcfun (device-property "IOHidDeviceGetProperty") :pointer
  (device :pointer)
  (key :pointer))

(cffi:defcfun (device-register-value-callback "IOHIDDeviceRegisterInputValueCallback") :void
  (device :pointer)
  (callback :pointer)
  (user :pointer))

(cffi:defcfun (device-set-value "IOHIDDeviceSetValue") io-return
  (device :pointer)
  (element :pointer)
  (value :pointer))

(cffi:defcfun (device-schedule-with-run-loop "IOHIDDeviceScheduleWithRunLoop") :void
  (device :pointer)
  (run-loop :pointer)
  (mode :pointer))

(cffi:defcfun (device-unschedule-from-run-loop "IOHIDDeviceUnscheduleFromRunLoop") :void
  (device :pointer)
  (run-loop :pointer)
  (mode :pointer))

(cffi:defcfun (value-element "IOHIDValueGetElement") :pointer
  (value :pointer))

(cffi:defcfun (value-int-value "IOHIDValueGetIntegerValue") :long
  (value :pointer))

(cffi:defcfun (value-timestamp "IOHIDValueGetTimeStamp") :uint64
  (value :pointer))

(cffi:defcfun (value-length "IOHIDValueGetLength") :long
  (value :pointer))

(cffi:defcfun (element-cookie "IOHIDElementGetCookie") :uint32
  (element :pointer))

(cffi:defcfun (element-type "IOHIDElementGetType") hid-element-type
  (element :pointer))

(cffi:defcfun (element-logical-min "IOHIDElementGetLogicalMin") :long
  (element :pointer))

(cffi:defcfun (element-logical-max "IOHIDElementGetLogicalMax") :long
  (element :pointer))

(cffi:defcfun (element-usage "IOHIDElementGetUsage") io-desktop-usage
  (element :pointer))

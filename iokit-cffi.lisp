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

(defcenum* (io-page :uint)
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

(defcenum* (io-desktop-usage :uint)
  (:undefined #x00)
  (:pointer #x01)   ; physical collection
  (:mouse #x02)   ; application collection
  (:joystick #x04)   ; application collection
  (:gamepad #x05)   ; application collection
  (:keyboard #x06)   ; application collection
  (:keypad #x07)   ; application collection
  (:multi-axis-controller #x08)   ; application collection
  (:x #x30)   ; dynamic value
  (:y #x31)   ; dynamic value
  (:z #x32)   ; dynamic value
  (:rx #x33)   ; dynamic value
  (:ry #x34)   ; dynamic value
  (:rz #x35)   ; dynamic value
  (:slider #x36)   ; dynamic value
  (:dial #x37)   ; dynamic value
  (:wheel #x38)   ; dynamic value
  (:hat-switch #x39)   ; dynamic value
  (:counted-buffer #x3a)   ; logical collection
  (:byte-count #x3b)   ; dynamic value
  (:motion-wake-up #x3c)   ; one-shot control
  (:start #x3d)   ; on/off control
  (:select #x3e)   ; on/off control
  (:vx #x40)   ; dynamic value
  (:vy #x41)   ; dynamic value
  (:vz #x42)   ; dynamic value
  (:vbrx #x43)   ; dynamic value
  (:vbry #x44)   ; dynamic value
  (:vbrz #x45)   ; dynamic value
  (:vno #x46)   ; dynamic value
  (:system-control #x80)   ; application collection
  (:system-powerdown #x81)   ; one-shot control
  (:system-sleep #x82)   ; one-shot control
  (:system-wakeup #x83)   ; one-shot control
  (:system-context-menu #x84)   ; one-shot control
  (:system-main-menu #x85)   ; one-shot control
  (:system-app-menu #x86)   ; one-shot control
  (:system-menu-help #x87)   ; one-shot control
  (:system-menu-exit #x88)   ; one-shot control
  (:system-menu-select #x89)   ; selector
  (:system-menu #x89)   ; selector
  (:system-menu-right #x8a)   ; re-trigger control
  (:system-menu-left #x8b)   ; re-trigger control
  (:system-menu-up #x8c)   ; re-trigger control
  (:system-menu-down #x8d)   ; re-trigger control
  (:dpad-up #x90)   ; on/off control
  (:dpad-down #x91)   ; on/off control
  (:dpad-right #x92)   ; on/off control
  (:dpad-left #x93)   ; on/off control
  (:reserved #xffff))

(defcenum* (io-simulation-usage :uint)
  (:undefined #x00)
  (:flight-simulation-device #x01)    ; application collection
  (:automobile-simulation-device #x02)    ; application collection
  (:tank-simulation-device #x03)    ; application collection
  (:spaceship-simulation-device #x04)    ; application collection
  (:submarine-simulation-device #x05)    ; application collection
  (:sailing-simulation-device #x06)    ; application collection
  (:motorcycle-simulation-device #x07)    ; application collection
  (:sports-simulation-device #x08)    ; application collection
  (:airplane-simulation-device #x09)    ; application collection
  (:helicopter-simulation-device #x0a)    ; application collection
  (:magiccarpet-simulation-device #x0b)    ; application collection
  (:bicycle-simulation-device #x0c)    ; application collection
  (:flight-control-stick #x20)    ; application collection
  (:flight-stick #x21)    ; application collection
  (:cyclic-control #x22)    ; physical collection
  (:cyclic-trim #x23)    ; physical collection
  (:flight-yoke #x24)    ; application collection
  (:track-control #x25)    ; physical collection
  (:aileron #xb0)    ; dynamic value
  (:aileron-trim #xb1)    ; dynamic value
  (:anti-torque-control #xb2)    ; dynamic value
  (:auto-pilot-enable #xb3)    ; on/off control
  (:chaff-release #xb4)    ; one-shot control
  (:collective-control #xb5)    ; dynamic value
  (:dive-brake #xb6)    ; dynamic value
  (:electronic-countermeasures #xb7)    ; on/off control
  (:elevator #xb8)    ; dynamic value
  (:elevator-trim #xb9)    ; dynamic value
  (:rudder #xba)    ; dynamic value
  (:throttle #xbb)    ; dynamic value
  (:flight-communications #xbc)    ; on/off control
  (:flare-release #xbd)    ; one-shot control
  (:landing-gear #xbe)    ; on/off control
  (:toe-brake #xbf)    ; dynamic value
  (:trigger #xc0)    ; momentary control
  (:weapons-arm #xc1)    ; on/off control
  (:weapons #xc2)    ; selector
  (:wing-flaps #xc3)    ; dynamic value
  (:accelerator #xc4)    ; dynamic value
  (:brake #xc5)    ; dynamic value
  (:clutch #xc6)    ; dynamic value
  (:shifter #xc7)    ; dynamic value
  (:steering #xc8)    ; dynamic value
  (:turret-direction #xc9)    ; dynamic value
  (:barrel-elevation #xca)    ; dynamic value
  (:dive-plane #xcb)    ; dynamic value
  (:ballast #xcc)    ; dynamic value
  (:bicycle-crank #xcd)    ; dynamic value
  (:handlebars #xce)    ; dynamic value
  (:front-brake #xcf)    ; dynamic value
  (:rear-brake #xd0)    ; dynamic value
  (:reserved #xffff))

(defcenum* (io-vr-usage :uint)
  (:undefined #x00)
  (:belt #x01) ; application collection
  (:body-suit #x02) ; application collection
  (:flexor #x03) ; physical collection
  (:glove #x04) ; application collection
  (:head-tracker #x05) ; physical collection
  (:head-mounted-display #x06) ; application collection
  (:hand-tracker #x07) ; application collection
  (:oculometer #x08) ; application collection
  (:vest #x09) ; application collection
  (:animatronic-device #x0a) ; application collection
  (:stereo-enable #x20) ; on/off control
  (:display-enable #x21) ; on/off control
  (:reserved #xffff))

(defcenum* (io-game-usage :uint)
  (:undefined #x00)
  (:3d-game-controller #x01) ; application collection
  (:pinball-device #x02) ; application collection
  (:gun-device #x03) ; application collection
  (:point-of-view #x20) ; physical collection
  (:turn-right-or-left #x21) ; dynamic value
  (:pitch-up-or-down #x22) ; dynamic value
  (:roll-right-or-left #x23) ; dynamic value
  (:move-right-or-left #x24) ; dynamic value
  (:move-forward-or-backward #x25) ; dynamic value
  (:move-up-or-down #x26) ; dynamic value
  (:lean-right-or-left #x27) ; dynamic value
  (:lean-forward-or-backward #x28) ; dynamic value
  (:height-of-pov #x29) ; dynamic value
  (:flipper #x2a) ; momentary control
  (:secondary-flipper #x2b) ; momentary control
  (:bump #x2c) ; momentary control
  (:new-game #x2d) ; one-shot control
  (:shootball #x2e) ; one-shot control
  (:player #x2f) ; one-shot control
  (:gun-bolt #x30) ; on/off control
  (:gun-clip #x31) ; on/off control
  (:gun #x32) ; selector
  (:gun-single-shot #x33) ; selector
  (:gun-burst #x34) ; selector
  (:gun-automatic #x35) ; selector
  (:gun-safety #x36) ; on/off control
  (:gamepad-fire-or-jump #x37) ; logical collection
  (:gamepad-trigger #x39) ; logical collection
  (:reserved #xffff))

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

(cffi:defcfun (device-property "IOHIDDeviceGetProperty") :pointer
  (device :pointer)
  (key :pointer))

(cffi:defcfun (register-value-callback "IOHIDDeviceRegisterInputValueCallback") :void
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

(cffi:defcfun (element-page-usage "IOHIDElementGetUsage") :uint32
  (element :pointer))

(cffi:defcfun (element-page "IOHIDElementGetUsagePage") io-page
  (element :pointer))

(defun element-usage (element)
  (let ((usage (element-page-usage element)))
    (case (element-page element)
      (:generic-desktop
       (cffi:foreign-enum-keyword 'io-desktop-usage usage))
      (:simulation
       (cffi:foreign-enum-keyword 'io-simulation-usage usage))
      (:vr
       (cffi:foreign-enum-keyword 'io-vr-usage usage))
      (:game
       (cffi:foreign-enum-keyword 'io-game-usage usage))
      (:button
       usage))))

(defun device-int-property (device key)
  (let ((prop (check-null (device-property device key))))
    (cffi:with-foreign-object (value :int32)
      (number-get-value prop :int32 value)
      (cffi:mem-ref value :int32))))

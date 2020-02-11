#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.impl)

(cffi:define-foreign-library iokit
  (T (:framework "IOKit")))

(defun kio-err (x)
  (flet ((err-system (x)
           (ash (logand x #x3f) 26))
         (err-sub (x)
           (ash (logand x #xFFF) 14)))
    (logior (err-system #x38) (err-sub 0) x)))

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

(cffi:defcfun (schedule-with-run-loop "IOHIDManagerScheduleWithRunLoop") :void
  (manager :pointer)
  (run-loop :pointer)
  (mode :pointer))

(cffi:defcfun (unschedule-from-run-loop "IOHIDManagerUnscheduleFromRunLoop") :void
  (manager :pointer)
  (run-loop :pointer)
  (mode :pointer))

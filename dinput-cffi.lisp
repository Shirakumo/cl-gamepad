#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.impl)

(cffi:define-foreign-library dinput
  (T (:default "dinput8")))

(cffi:define-foreign-library ole32
  (T (:default "Ole32")))

(defconstant CP-UTF8 65001)
(defconstant CLSCTX-ALL 23)
(defconstant DINPUT-VERSION #x0800)
(defconstant MAX-PATH 260)

(define-condition win32-error (error)
  ((function-name :initarg :function-name :reader function-name)
   (code :initarg :code :reader code))
  (:report (lambda (c s) (format s "File select operation failed!~%The call to~%  ~a~%returned with unexpected result code ~a."
                                 (function-name c) (code c)))))

(defmacro check-return (value-form &rest expected)
  (let ((value (gensym "VALUE")))
    `(let ((,value ,value-form))
       (if (find ,value ',(or expected '(:ok)))
           ,value
           (error 'win32-error :code ,value :function-name ',(first value-form))))))

(cffi:defcenum coinit
  (:apartment-threaded #x2)
  (:multi-threaded #x0)
  (:disable-ole1dde #x4)
  (:speed-over-memory #x8))

(cffi:defcenum hresult
  (:ok #x00000000)
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
  (:already-initialized 2290679810)
  (:bad-pointer 2147500035)
  (:bufduration-period-not-equal 2290679827)
  (:buffer-error 2290679832)
  (:buffer-operation-pending 2290679819)
  (:buffer-size-error 2290679830)
  (:buffer-size-not-aligned 2290679833)
  (:buffer-too-large 2290679814)
  (:cpuusage-exceeded 2290679831)
  (:device-in-use 2290679818)
  (:device-invalidated 2290679812)
  (:endpoint-create-failed 2290679823)
  (:exclusive-mode-not-allowed 2290679822)
  (:invalid-device-period 2290679840)
  (:invalid-size 2290679817)
  (:not-initialized 2290679809)
  (:out-of-order 2290679815)
  (:service-not-running 2290679824)
  (:unsupported-format 2290679816)
  (:wrong-endpoint-type 2290679811)
  (:class-not-registered 2147746132)
  (:no-aggregation 2147746064))

(cffi:defcstruct (com :conc-name ||)
  (vtbl :pointer))

(cffi:defcstruct (guid :conc-name guid-)
  (data1 dword)
  (data2 word)
  (data3 word)
  (data4 :uint8 :count 8))

(cffi:defcfun (co-initialize "CoInitializeEx") hresult
  (nullable :pointer)
  (init coinit))

(cffi:defcfun (co-uninitialize "CoUninitialize") :void)

(cffi:defcfun (co-create-instance "CoCreateInstance") hresult
  (rclsid :pointer)
  (punkouter :pointer)
  (dwclscontext dword)
  (riid :pointer)
  (ppv :pointer))

(cffi:defcfun (wide-char-to-multi-byte "WideCharToMultiByte") :int
  (code-page :uint)
  (flags dword)
  (wide-char-str :pointer)
  (wide-char :int)
  (multi-byte-str :pointer)
  (multi-byte :int)
  (default-char :pointer)
  (used-default-char :pointer))

(cffi:defcfun (multi-byte-to-wide-char "MultiByteToWideChar") :int
  (code-page :uint)
  (flags dword)
  (multi-byte-str :pointer)
  (multi-byte :int)
  (wide-char-str :pointer)
  (wide-char :int))

(defun wstring->string (pointer)
  (let ((bytes (wide-char-to-multi-byte CP-UTF8 0 pointer -1 (cffi:null-pointer) 0 (cffi:null-pointer) (cffi:null-pointer))))
    (cffi:with-foreign-object (string :uchar bytes)
      (wide-char-to-multi-byte CP-UTF8 0 pointer -1 string bytes (cffi:null-pointer) (cffi:null-pointer))
      (cffi:foreign-string-to-lisp string :encoding :utf-8))))

(defun string->wstring (string)
  (cffi:with-foreign-string (string string)
    (let* ((chars (multi-byte-to-wide-char CP-UTF8 0 string -1 (cffi:null-pointer) 0))
           (pointer (cffi:foreign-alloc :uint16 :count chars)))
      (multi-byte-to-wide-char CP-UTF8 0 string -1 pointer chars)
      pointer)))

(defun com-release (pointer)
  (cffi:foreign-funcall-pointer
   (cffi:mem-aref (vtbl pointer) :pointer 2)
   ()
   :pointer pointer
   :unsigned-long))

(defun make-guid (d1 d2 d3 &rest d4)
  (let ((ptr (cffi:foreign-alloc '(:struct guid))))
    (setf (guid-data1 ptr) d1)
    (setf (guid-data2 ptr) d2)
    (setf (guid-data3 ptr) d3)
    (loop for i from 0 below 8
          for d in d4
          do (setf (cffi:mem-aref (cffi:foreign-slot-pointer ptr '(:struct guid) 'data4) :uint8 i)
                   d))
    ptr))

(defun guid-string (guid)
  (let ((data4 (cffi:foreign-slot-pointer guid '(:struct guid) 'data4)))
    (with-output-to-string (out)
      (format out "~8,'0x-~4,'0x-~4,'0x-~2,'0x~2,'0x-"
              (guid-data1 guid)
              (guid-data2 guid)
              (guid-data3 guid)
              (cffi:mem-aref data4 :uint8 0)
              (cffi:mem-aref data4 :uint8 1))
      (dotimes (i 6)
        (format out "~2,'0x" (cffi:mem-aref data4 :uint8 (+ 2 i)))))))

(defun guid-integer (guid)
  (let ((integer 0)
        (data4 (cffi:foreign-slot-pointer guid '(:struct guid) 'data4)))
    (declare (optimize speed))
    (declare (type (unsigned-byte 128) integer))
    (setf (ldb (cl:byte 32 96) integer) (guid-data1 guid))
    (setf (ldb (cl:byte 16 80) integer) (guid-data2 guid))
    (setf (ldb (cl:byte 16 64) integer) (guid-data3 guid))
    (dotimes (i 8)
      (setf (ldb (cl:byte 8 (- 56 (* i 8))) integer) (cffi:mem-aref data4 :uint8 i)))
    integer))

(defun integer-guid (integer)
  (make-guid (ldb (cl:byte 32 96) integer)
             (ldb (cl:byte 16 80) integer)
             (ldb (cl:byte 16 64) integer)
             (ldb (cl:byte 8 56) integer)
             (ldb (cl:byte 8 48) integer)
             (ldb (cl:byte 8 40) integer)
             (ldb (cl:byte 8 32) integer)
             (ldb (cl:byte 8 24) integer)
             (ldb (cl:byte 8 16) integer)
             (ldb (cl:byte 8 8) integer)
             (ldb (cl:byte 8 0) integer)))

(defmacro define-comfun ((struct method &rest options) return-type &body args)
  (let* ((*print-case* (readtable-case *readtable*))
         (structg (gensym "STRUCT"))
         (name (intern (format NIL "~a-~a" struct method))))
    `(progn
       (declaim (inline ,name))
       (defun ,name (,structg ,@(mapcar #'first args))
         (cffi:foreign-funcall-pointer
          (,(intern (format NIL "%~a" name))
           (vtbl ,structg))
          ,options
          :pointer ,structg
          ,@(loop for (name type) in args
                  collect type collect name)
          ,return-type)))))

(defmacro define-comstruct (name &body methods)
  (let ((methods (list* `(query-interface hresult)
                        `(add-ref :unsigned-long)
                        `(release :unsigned-long)
                        methods)))
    `(progn
       (cffi:defcstruct (,name :conc-name ,(format NIL "%~a-" name))
         ,@(loop for method in methods
                 collect (list (first method) :pointer)))

       ,@(loop for (method return . args) in methods
               collect `(define-comfun (,name ,method) ,return
                          ,@args)))))

(defvar GUID-DEVINTERFACE-HID (make-guid #x4D1E55B2 #xF16F #x11CF #x88 #xCB #x00 #x11 #x11 #x00 #x00 #x30))
(defvar IID-IDIRECTINPUT8 (make-guid #xBF798031 #x483A #x4DA2 #xAA #x99 #x5D #x64 #xED #x36 #x97 #x00))
(defvar DIPROP-GRANULARITY (cffi:make-pointer 3))
(defvar DIPROP-RANGE (cffi:make-pointer 4))
(defvar DIPROP-DEADZONE (cffi:make-pointer 5))
(defvar HWND-MESSAGE (cffi:make-pointer (- (ash 1 #+64-bit 64 #-64-bit 32) 3)))

(cffi:defcenum (device-type dword)
  (:all 0)
  (:device 1)
  (:pointer 2)
  (:keyboard 3)
  (:game-controller 4))

(cffi:defbitfield (device-flags dword)
  (:all-devices       #x00000000)
  (:attached-only     #x00000001)
  (:force-feedback    #x00000100)
  (:include-aliases   #x00010000)
  (:include-phantoms  #x00020000)
  (:include-hidden    #x00040000))

(cffi:defbitfield (object-flags dword)
  (:all           #x00000000)
  (:relative-axis #x00000001)
  (:absolute-axis #x00000002)
  (:axis          #x00000003)
  (:push-button   #x00000004)
  (:toggle-button #x00000008)
  (:button        #x0000000C)
  (:pov           #x00000010)
  (:collection    #x00000040)
  (:nodata        #x00000080)
  (:ff-actuator   #x01000000)
  (:ff-trigger    #x02000000))

(cffi:defbitfield (cooperation-flags dword)
  (:exclusive    #x01)
  (:nonexclusive #x02)
  (:foreground   #x04)
  (:background   #x08)
  (:no-win-key   #x10))

(cffi:defbitfield (wait-flags dword)
  (:normal #x0)
  (:wait-all #x1)
  (:alertable #x2)
  (:input-available #x4))

(cffi:defbitfield (wake-flags dword)
  (:all-events 1215)
  (:all-input 1279)
  (:all-post-message 256)
  (:hotkey 128)
  (:input 1031)
  (:key 1)
  (:mouse 6)
  (:mouse-button 4)
  (:mouse-move 2)
  (:paint 32)
  (:post-message 8)
  (:raw-input 1024)
  (:send-message 64)
  (:timer 16))

(cffi:defcenum (enumerate-flag word)
  (:stop 0)
  (:continue 1))

(cffi:defcenum (property-header-flag dword)
  (:device 0)
  (:by-offset 1)
  (:by-id 2)
  (:by-usage 3))

(cffi:defcenum (win-device-type dword)
  (:oem              #x00000000)
  (:device-node      #x00000001)
  (:volume           #x00000002)
  (:port             #x00000003)
  (:net              #x00000004)
  (:device-interface #x00000005)
  (:handle           #x00000006))

(cffi:defcenum (wparam #+64-bit :uint64 #-64-bit :unsigned-long)
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

(cffi:defcenum (window-message :uint)
  (:device-change #x0219))

(cffi:defcstruct (device-instance :conc-name device-instance-)
  (size dword)
  (guid (:struct guid))
  (product (:struct guid))
  (type dword)
  (instance-name tchar :count #.MAX-PATH)
  (product-name tchar :count #.MAX-PATH)
  (ff-driver (:struct guid))
  (usage-page word)
  (usage word))

(cffi:defcstruct (enum-user-data :conc-name enum-user-data-)
  (directinput :pointer)
  (device-array :pointer)
  (device-count :uint8))

(cffi:defcstruct (object-data-format :conc-name object-data-format-)
  (guid :pointer)
  (ofs dword)
  (type dword)
  (flags dword))

(cffi:defcstruct (data-format :conc-name data-format-)
  (size dword)
  (object-size dword)
  (flags dword)
  (data-size dword)
  (object-count dword)
  (object-data-format :pointer))

(cffi:defcstruct (joystate :conc-name joystate-)
  (l-x long)
  (l-y long)
  (l-z long)
  (r-x long)
  (r-y long)
  (r-z long)
  (slider long :count 2)
  (pov dword :count 4)
  (buttons byte :count 32))

(cffi:defcvar (data-format-joystick "c_dfDIJoystick") (:struct data-format))

(cffi:defcstruct (device-capabilities :conc-name device-capabilities-)
  (size dword)
  (flags dword)
  (device-type dword)
  (axes dword)
  (buttons dword)
  (povs dword)
  (sample-period dword)
  (min-time-resolution dword)
  (firmware-revision dword)
  (hardware-revision dword)
  (driver-version dword))

(cffi:defcstruct (property-header :conc-name property-hader-)
  (size dword)
  (header-size dword)
  (object dword)
  (how property-header-flag))

(cffi:defcstruct (property-range :conc-name property-range-)
  (size dword)
  (header-size dword)
  (type dword)
  (how property-header-flag)
  ;; ^ (header (:struct property-header))
  (min long)
  (max long))

(cffi:defcstruct (property-dword :conc-name property-dword-)
  (size dword)
  (header-size dword)
  (type dword)
  (how property-header-flag)
  ;; ^ (header (:struct property-header))
  (data dword))

(cffi:defcstruct (device-object-instance :conc-name device-object-instance-)
  (size dword)
  (guid (:struct guid))
  (ofs dword)
  (type dword)
  (flags dword)
  (name wchar :count #.MAX-PATH)
  (ff-max-force dword)
  (ff-force-resolution dword)
  (collection-number word)
  (designator-index word)
  (usage-page word)
  (usage word)
  (dimension dword)
  (exponent word)
  (reserved word))

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

(cffi:defcstruct (broadcast-device-interface :conc-name broadcast-device-interface-)
  (size dword)
  (device-type win-device-type)
  (reserved dword)
  (guid (:struct guid))
  (name wchar))

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

(cffi:defcfun (get-module-handle "GetModuleHandleW") :pointer
  (module-name :pointer))

(cffi:defcfun (create-direct-input "DirectInput8Create") hresult
  (instance :pointer)
  (version dword)
  (refiid :pointer)
  (interface :pointer)
  (aggregation :pointer))

(cffi:defcfun (register-class "RegisterClassExW") word
  (class :pointer))

(cffi:defcfun (unregister-class "UnregisterClass") :void
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

(cffi:defcfun (wait-for-multiple-objects "MsgWaitForMultipleObjectsEx") dword
  (count dword)
  (handles :pointer)
  (milliseconds dword)
  (wake-mask wake-flags)
  (flags wait-flags))

(cffi:defcfun (peek-message "PeekMessageW") :bool
  (message :pointer)
  (window :pointer)
  (filter-min :uint)
  (filter-max :uint)
  (remove-msg :uint))

(cffi:defcfun (get-message "GetMessage") :bool
  (message :pointer)
  (window :pointer)
  (filter-min :uint)
  (filter-max :uint))

(cffi:defcfun (translate-message "TranslateMessage") :bool
  (message :pointer))

(cffi:defcfun (dispatch-message "DispatchMessage") :pointer
  (message :pointer))

(cffi:defcfun (create-event "CreateEventW") :pointer
  (attributes :pointer)
  (manual-reset :bool)
  (initial-state :bool)
  (name :pointer))

(cffi:defcfun (close-handle "CloseHandle") :void
  (handle :pointer))

(define-comstruct directinput
  (create-device hresult (guid :pointer) (device :pointer) (outer :pointer))
  (enum-devices hresult (type device-type) (callback :pointer) (user :pointer) (flags device-flags))
  (get-device-status hresult (instance :pointer))
  (run-control-panel hresult (owner :pointer) (flags dword))
  (initialize hresult (instance :pointer) (version dword))
  (find-device hresult (guid :pointer) (name :pointer) (instance :pointer))
  (enum-devices-by-semantics hresult (user-name :pointer) (action-format :pointer) (callback :pointer) (user :pointer) (flags dword))
  (configure-devices hresult (callback :pointer) (params :pointer) (flags dword) (user :pointer)))

(define-comstruct device
  (get-capabilities hresult (caps :pointer))
  (enum-objects hresult (callback :pointer) (user :pointer) (flags object-flags))
  (get-property hresult (property :pointer) (header :pointer))
  (set-property hresult (property :pointer) (header :pointer))
  (acquire hresult)
  (unacquire hresult)
  (get-device-state hresult (data dword) (data* :pointer))
  (get-device-data hresult (object-data dword) (object-data* :pointer) (inout :pointer) (flags dword))
  (set-data-format hresult (format :pointer))
  (set-event-notification hresult (event :pointer))
  (set-cooperative-level hresult (hwnd :pointer) (flags cooperation-flags))
  (get-object-info hresult (instance :pointer) (object dword) (how dword))
  (get-device-info hresult (instance :pointer))
  (run-control-panel hresult (owner :pointer) (flags dword))
  (initialize hresult (instance :pointer) (version dword) (guid :pointer))
  (create-effect hresult (guid :pointer) (effect :pointer) (input-effect :pointer) (user :pointer))
  (enum-effects hresult (callback :pointer) (user :pointer) (type dword))
  (get-effect-info hresult (info :pointer) (guid :pointer))
  (get-force-feedback-state hresult (out :pointer))
  (send-force-feedback-command hresult (flags dword))
  (enum-created-effect-objects hresult (callback :pointer) (user :pointer) (flags dword))
  (escape hresult (escape :pointer))
  (poll hresult)
  (send-device-data hresult (object-data dword) (object-data* :pointer) (inout :pointer) (flags dword))
  (enum-effects-in-file hresult (file-name :pointer) (callback :pointer) (user :pointer) (flags dword))
  (write-effect-to-file hresult (file-name :pointer) (entries dword) (effects :pointer) (flags dword))
  (bild-action-map hresult (format :pointer) (user-name :pointer) (flags dword))
  (set-action-map hresult (format :pointer) (user-name :pointer) (flags dword))
  (get-image-info hresult (image-info :pointer)))

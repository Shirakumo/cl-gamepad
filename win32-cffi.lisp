#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.impl)

(cffi:define-foreign-library ole32
  (T (:default "Ole32")))

(defconstant CP-UTF8 65001)
(defconstant CLSCTX-ALL 23)
(defconstant MAX-PATH 260)
(defvar HWND-MESSAGE (cffi:make-pointer (- (ash 1 #+64-bit 64 #-64-bit 32) 3)))

(cffi:defctype dword :uint32)
(cffi:defctype word :uint16)
(cffi:defctype long :int32)
(cffi:defctype short :int16)
(cffi:defctype byte :uint8)
(cffi:defctype tchar :uint8)
(cffi:defctype wchar :uint16)

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

(cffi:defcfun (get-module-handle "GetModuleHandleW") :pointer
  (module-name :pointer))

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

(cffi:defcfun (wait-for-single-object "WaitForSingleObjectEx") dword
  (handle :pointer)
  (milliseconds dword)
  (alertable :bool))

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

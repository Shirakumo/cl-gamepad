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

(cffi:defctype tchar :uint8)

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

(defvar IID-IDIRECTINPUT8 (make-guid #xBF798031 #x483A #x4DA2 #xAA #x99 #x5D #x64 #xED #x36 #x97 #x00))

(cffi:defcenum (device-type dword)
  (:all 0)
  (:device 1)
  (:pointer 2)
  (:keyboard 3)
  (:game-controller 4))

(cffi:defcenum (device-flags dword)
  (:all-devices       #x00000000)
  (:attached-only     #x00000001)
  (:force-feedback    #x00000100)
  (:include-aliases   #x00010000)
  (:include-phantoms  #x00020000)
  (:include-hidden    #x00040000))

(cffi:defbitfield (cooperation-flags dword)
  (:exclusive    #x01)
  (:nonexclusive #x02)
  (:foreground   #x04)
  (:background   #x08)
  (:no-win-key   #x10))

(cffi:defcenum (enumerate-flag word)
  (:stop 0)
  (:continue 1))

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

(cffi:defcfun (get-module-handle "GetModuleHandleW") :pointer
  (module-name :pointer))

(cffi:defcfun (create-direct-input "DirectInput8Create") hresult
  (instance :pointer)
  (version dword)
  (refiid :pointer)
  (interface :pointer)
  (aggregation :pointer))

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
  (enum-objects hresult (callback :pointer) (user :pointer) (flags dword))
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

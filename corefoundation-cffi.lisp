(in-package #:org.shirakumo.fraf.gamepad.impl)

(cffi:define-foreign-library corefoundation
  (T (:framework "CoreFoundation")))

(cffi:defcenum (hresult :uint32)
  (:ok                        #x00000000)
  (:false                     #x00000001)
  (:download-skipped          #x00000003)
  (:effect-restarted          #x00000004)
  (:truncated                 #x00000008)
  (:truncated-and-restarted   #x0000000c)
  ;;(:device-not-reg            REGDB_E_CLASSNOTREG)
  (:invalid-param             #x80000003)
  (:no-interface              #x80000004)
  (:generic                   #x80000008)
  (:out-of-memory             #x80000002)
  (:unsupported               #x80000001)
  (:pending                   #x8000000a)
  (:device-full               #x80040201)
  (:more-data                 #x80040202)
  (:not-downloaded            #x80040203)
  (:has-effects               #x80040204)
  (:incomplete-effect         #x80040206)
  (:effect-playing            #x80040208)
  (:unplugged                 #x80040209)
  (:invalid-download-id       #x80040300)
  (:device-paused             #x80040301)
  (:internal                  #x80040302)
  (:effect-type-mismatch      #x80040303)
  (:unsupported-axis          #x80040304)
  (:no-tinitialized           #x80040305)
  (:effect-type-not-supported #x80040306)
  (:device-released           #x80040307))

(cffi:defcenum (run-loop-result :int32)
  (:finished 1)
  (:stopped 2)
  (:timed-out 3)
  (:handled-source 4))

(cffi:defcenum (number-type :uint32)
  (:int8      1)
  (:int16     2)
  (:int32     3)
  (:int64     4)
  (:float32   5)
  (:float64   6)
  (:char      7)
  (:short     8)
  (:int       9)
  (:long      10)
  (:long-long 11)
  (:float     12)
  (:double    13)
  (:index     14)
  (:integer   15)
  (:cg-float  16))

(cffi:defcfun (string-type-id "CFStringGetTypeID") :ulong)

(cffi:defcenum (string-encoding :uint32)
  (:utf-8 #x08000100))

(cffi:defcfun (%release "CFRelease") :void
  (object :pointer))

(cffi:defcfun (type-id "CFGetTypeID") :ulong
  (object :pointer))

(cffi:defcfun (%create-number "CFNumberCreate") :pointer
  (allocator :pointer)
  (type number-type)
  (value :pointer))

(cffi:defcfun (number-get-value "CFNumberGetValue") :void
  (object :pointer)
  (type number-type)
  (value :pointer))

(cffi:defcfun (%create-dictionary "CFDictionaryCreate") :pointer
  (allocator :pointer)
  (keys :pointer)
  (values :pointer)
  (count :long)
  (key-callbacks :pointer)
  (value-callbacks :pointer))

(cffi:defcfun (%create-array "CFArrayCreate") :pointer
  (allocator :pointer)
  (values :pointer)
  (count :long)
  (callbacks :pointer))

(cffi:defcfun (%create-string "CFStringCreateWithCString") :pointer
  (allocator :pointer)
  (string :string)
  (encoding string-encoding))

(cffi:defcfun (string-get-length "CFStringGetLength") :long
  (string :pointer))

(cffi:defcfun (string-get-cstring "CFStringGetCString") :bool
  (string :pointer)
  (buffer :pointer)
  (length :long)
  (encoding string-encoding))

(cffi:defcfun (string-get-cstring-ptr "CFStringGetCStringPtr") :pointer
  (string :pointer)
  (encoding string-encoding))

(cffi:defcfun (cfstr "__CFStringMakeConstantString") :pointer
  (string :string))

(cffi:defcfun (set-get-count "CFSetGetCount") :long
  (set :pointer))

(cffi:defcfun (set-get-values "CFSetGetValues") :void
  (set :pointer)
  (values :pointer))

(cffi:defcfun (get-uuid "CFUUIDGetConstantUUIDWithBytes") :pointer
  (allocator :pointer)
  (byte0 :uint8)
  (byte1 :uint8)
  (byte2 :uint8)
  (byte3 :uint8)
  (byte4 :uint8)
  (byte5 :uint8)
  (byte6 :uint8)
  (byte7 :uint8)
  (byte8 :uint8)
  (byte9 :uint8)
  (byte10 :uint8)
  (byte11 :uint8)
  (byte12 :uint8)
  (byte13 :uint8)
  (byte14 :uint8)
  (byte15 :uint8))

(cffi:defcfun (run-loop "CFRunLoopRunInMode") run-loop-result
  (mode :pointer)
  (seconds :double)
  (return-after-source-handled :bool))

(cffi:defcfun (get-current-run-loop "CFRunLoopGetCurrent") :pointer)

(define-condition macos-error (gamepad:gamepad-error)
  ((function-name :initarg :function-name :initform NIL :reader function-name)
   (message :initarg :message :initform NIL :reader message))
  (:report (lambda (c s) (format s "The call ~@[to~%  ~a~%~]failed.~@[~%  ~a~]"
                                 (function-name c) (message c)))))

(declaim (inline macos-error))
(defun macos-error (&key function-name message)
  (error 'macos-error :function-name function-name :message message))

(defun release (&rest objects)
  (dolist (object objects)
    (unless (cffi:null-pointer-p object)
      (%release object))))

(defmacro check-null (form)
  (let ((value (gensym "VALUE")))
    `(let ((,value ,form))
       (if (cffi:null-pointer-p ,value)
           (macos-error :function-name ',(first form))
           ,value))))

(defun create-number (type number)
  (cffi:with-foreign-object (value type)
    (setf (cffi:mem-ref value type) number)
    (check-null (%create-number (cffi:null-pointer) type value))))

(defun create-dictionary (pairs)
  (let ((count (length pairs)))
    (cffi:with-foreign-objects ((keys :pointer count)
                                (values :pointer count))
      (loop for i from 0
            for (k . v) in pairs
            do (setf (cffi:mem-aref keys :pointer i) k)
               (setf (cffi:mem-aref values :pointer i) v))
      (check-null (%create-dictionary (cffi:null-pointer) keys values count (cffi:null-pointer) (cffi:null-pointer))))))

(defun create-array (entries)
  (let ((count (length entries)))
    (cffi:with-foreign-object (data :pointer count)
      (loop for i from 0
            for entry in entries
            do (setf (cffi:mem-aref data :pointer i) entry))
      (check-null (%create-array (cffi:null-pointer) data count (cffi:null-pointer))))))

(defun create-string (string)
  (check-null (%create-string (cffi:null-pointer) string :utf-8)))

(defun cfstring->string (pointer)
  (let ((buffer (string-get-cstring-ptr pointer :utf-8)))
    (cond ((cffi:null-pointer-p buffer)
           (let ((length (1+ (* 2 (string-get-length pointer)))))
             (if (= 0 length)
                 (make-string 0)
                 (cffi:with-foreign-object (buffer :uint8 length)
                   (if (string-get-cstring pointer buffer length :utf-8)
                       (cffi:foreign-string-to-lisp buffer :encoding :utf-8)
                       (macos-error :message "Failed to convert string to lisp!"))))))
          (T
           (cffi:foreign-string-to-lisp buffer :encoding :utf-8)))))

(defmacro with-cf-objects (bindings &body body)
  `(let ,(loop for binding in bindings
               collect (list (first binding) `(cffi:null-pointer)))
     (unwind-protect
          (progn
            ,@(loop for (name init) in bindings
                    collect `(setf ,name ,init))
            ,@body)
       (release ,@(nreverse (mapcar #'first bindings))))))

(defmacro define-lazy-constant (name init)
  `(let (value)
     (defun ,name ()
       (or value (setf value ,init)))
     (define-symbol-macro ,name (,name))))

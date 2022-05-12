#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.impl)

(defvar *devices-need-refreshing* T)
(defvar *device-table* (make-hash-table :test 'equal))
(defvar *directinput*)
(defvar *device-notifier*)
(defvar *poll-event*)
(defvar *xinput-taken* #*0000)
(defconstant EVENT-BUFFER-COUNT 32)

;;; error helpers
(define-condition win32-error (com:win32-error gamepad:gamepad-error)
  ())

(defmacro check-errno (predicate &body cleanup)
  `(unless ,predicate
     ,@cleanup
     (let ((errno (com-cffi:get-last-error)))
       (com:win32-error errno :type 'win32-error))))

(defmacro check-return (value-form &rest expected)
  (let ((value (gensym "VALUE")))
    `(let ((,value ,value-form))
       (if (find ,value ',(or expected '(:ok)))
           ,value
           (com:win32-error ,value :function-name ',(first value-form) :type 'win32-error)))))

(defstruct (device-notifier
            (:constructor make-device-notifier (class window notification))
            (:copier NIL)
            (:predicate NIL))
  (notification)
  (window)
  (class))

(cffi:defcallback device-change :pointer ((window :pointer) (message window-message) (wparam wparam) (lparam :pointer))
  (case message
    (:device-change
     (when (and (or (eql :device-arrival wparam)
                    (eql :device-remove-complete wparam))
                (eql :device-interface (broadcast-device-interface-device-type lparam)))
       (setf *devices-need-refreshing* T))))
  (default-window-handler window message wparam lparam))

(cffi:defcallback enum-devices enumerate-flag ((device :pointer) (user :pointer))
  (let* ((idx (enum-user-data-device-count user))
         (source (cffi:foreign-slot-pointer device '(:struct device-instance) 'guid))
         (target (cffi:mem-aptr (enum-user-data-device-array user) 'com:guid idx)))
    ;; GUID is 128 bits, copy in two uint64 chunks.
    (setf (cffi:mem-aref target :uint64 0) (cffi:mem-aref source :uint64 0))
    (setf (cffi:mem-aref target :uint64 1) (cffi:mem-aref source :uint64 1))
    (setf (enum-user-data-device-count user) (1+ idx))
    (if (< idx 255)
        :continue
        :stop)))

(cffi:defcallback enum-objects enumerate-flag ((object :pointer) (device :pointer))
  (cffi:with-foreign-object (range '(:struct property-range))
    (setf (property-range-size range) (cffi:foreign-type-size '(:struct property-range)))
    (setf (property-range-header-size range) (cffi:foreign-type-size '(:struct property-header)))
    (setf (property-range-how range) :by-id)
    (setf (property-range-type range) (device-object-instance-type object))
    ;; One byte of range
    (setf (property-range-min range) -32768)
    (setf (property-range-max range) +32767)
    (device-set-property device DIPROP-RANGE range))
  (cffi:with-foreign-object (dword '(:struct property-dword))
    (setf (property-dword-size dword) (cffi:foreign-type-size '(:struct property-dword)))
    (setf (property-dword-header-size dword) (cffi:foreign-type-size '(:struct property-header)))
    (setf (property-dword-how dword) :by-id)
    (setf (property-dword-type dword) (device-object-instance-type object))
    ;; No dead zone, handled in user code
    (setf (property-dword-data dword) 0)
    (device-set-property device DIPROP-DEADZONE dword))
  :continue)

(defun guid-vendor (guid)
  (+ (aref (com:bytes guid) 0)
     (ash (aref (com:bytes guid) 1) 8)))

(defun guid-product (guid)
  (+ (aref (com:bytes guid) 2)
     (ash (aref (com:bytes guid) 3) 8)))

(defun dev-xinput-p (guid)
  (or (loop for known in (list IID-VALVE-STREAMING-GAMEPAD
                               IID-X360-WIRED-GAMEPAD
                               IID-X360-WIRELESS-GAMEPAD)
            thereis (com:guid= known guid))
      (cffi:with-foreign-object (count :uint)
        (when (<= 0 (get-raw-input-device-list (cffi:null-pointer) count (cffi:foreign-type-size '(:struct raw-input-device-list))))
          (cffi:with-foreign-objects ((list '(:struct raw-input-device-list) (cffi:mem-ref count :uint))
                                      (info '(:struct hid-device-info))
                                      (name :uint16 256)
                                      (size :uint))
            (when (< 0 (get-raw-input-device-list list count (cffi:foreign-type-size '(:struct raw-input-device-list))))
              (loop for i from 0 below (cffi:mem-ref count :uint)
                    for entry = (cffi:mem-aref list '(:struct raw-input-device-list) i)
                    thereis (and (eql :hid (getf entry 'type))
                                 ;; Compare device ID to check if this is our product
                                 (setf (cffi:mem-ref size :uint) (cffi:foreign-type-size '(:struct hid-device-info)))
                                 (<= 0 (get-raw-input-device-info (getf entry 'device) :device-info info size))
                                 (= (hid-device-info-vendor-id info) (guid-vendor guid))
                                 (= (hid-device-info-product-id info) (guid-product guid))
                                 ;; Check name to see if it contains IG_, an identifier for xbox gamepads.
                                 (setf (cffi:mem-ref size :uint) 256)
                                 (<= 0 (get-raw-input-device-info (getf entry 'device) :device-name name size))
                                 (search "IG_" (com:wstring->string name))))))))))

(defun guess-xinput-id (guid)
  (when (dev-xinput-p guid)
    (cffi:with-foreign-object (capabilities '(:struct xcapabilities))
      (loop for i from 0 below 4
            when (and (= 0 (sbit *xinput-taken* i))
                      (eql :ok (get-xcapabilities i 0 capabilities)))
            return i))))

(defclass device (gamepad:device)
  ((dev :initarg :dev :reader dev)
   (guid :initarg :guid :reader guid)
   (xinput :initarg :xinput :initform NIL :reader xinput)
   (poll-device :initarg :poll-device :initform NIL :reader poll-device-p)
   (effect :initarg :effect :reader effect)
   (private-button-state :initform (make-array (length +labels+) :element-type 'bit) :reader private-button-state)
   ;; Need extra space since POVs take up twice as much room.
   (private-axis-state :initform (make-array (+ 4 (length +labels+)) :element-type 'single-float) :reader private-axis-state)))

(defmethod initialize-instance :after ((device device) &key)
  (when (xinput device)
    (setf (sbit *xinput-taken* (xinput device)) 1)))

(defun close-device (device)
  (when (xinput device)
    (setf (sbit *xinput-taken* (xinput device)) 0))
  (when (effect device)
    (effect-unload (effect device))
    (setf (slot-value device 'effect) NIL))
  (when (dev device)
    (device-unacquire (dev device))
    (device-set-event-notification (dev device) (cffi:null-pointer))
    (com:release (dev device)))
  (slot-makunbound device 'dev)
  (remhash (com:guid-string (guid device)) *device-table*))

(defun make-effect (dev)
  (cffi:with-foreign-objects ((ff-effect '(:struct ff-effect))
                              (ff-constant '(:struct ff-constant))
                              (ff-ramp '(:struct ff-ramp))
                              (ff-periodic '(:struct ff-periodic))
                              (effect :pointer)
                              (axes :long)
                              (directions :long))
    (setf (cffi:mem-ref axes :long) 0)
    (setf (cffi:mem-ref directions :long) 0)
    (setf (ff-constant-magnitude ff-constant) 10000)
    (setf (ff-ramp-start ff-ramp) 10000)
    (setf (ff-ramp-end ff-ramp) 10000)
    (setf (ff-periodic-magnitude ff-periodic) 10000)
    (setf (ff-periodic-offset ff-periodic) 0)
    (setf (ff-periodic-phase ff-periodic) 0)
    (setf (ff-periodic-period ff-periodic) 1)
    (setf (ff-effect-size ff-effect) (cffi:foreign-type-size '(:struct ff-effect)))
    (setf (ff-effect-flags ff-effect) '(:cartesian :object-offsets))
    (setf (ff-effect-duration ff-effect) 100000)
    (setf (ff-effect-sample-period ff-effect) 0)
    (setf (ff-effect-gain ff-effect) 10000)
    (setf (ff-effect-trigger-button ff-effect) #xFFFFFFFF)
    (setf (ff-effect-trigger-repeat-interval ff-effect) 0)
    (setf (ff-effect-axe-count ff-effect) 1)
    (setf (ff-effect-axe-identifiers ff-effect) axes)
    (setf (ff-effect-axe-directions ff-effect) directions)
    (setf (ff-effect-envelope ff-effect) (cffi:null-pointer))
    (setf (ff-effect-specific-size ff-effect) (cffi:foreign-type-size '(:struct ff-constant)))
    (setf (ff-effect-start-delay ff-effect) 0)
    (flet ((try-effect (guid ptr)
             (setf (ff-effect-specific ff-effect) ptr)
             (let ((value (device-create-effect dev guid ff-effect effect (cffi:null-pointer))))
               (case value
                 (:ok (cffi:mem-ref effect :pointer))
                 ;;(:device-full) ; FIXME: Empty device and retry.
                 (:not-implemented)
                 (T (com:win32-error value :function-name 'device-create-effect :type 'win32-error))))))
      (or (try-effect GUID-CONSTANT-FORCE ff-constant)
          (try-effect GUID-RAMP-FORCE ff-ramp)
          (try-effect GUID-SINE ff-periodic)))))

(defun make-device-from-guid (guid)
  (let ((dev (cffi:with-foreign-objects ((dev :pointer))
               (let ((ret (directinput-create-device *directinput* guid dev (cffi:null-pointer))))
                 (case ret
                   (:ok (cffi:mem-ref dev :pointer))
                   (:device-not-reg (return-from make-device-from-guid))
                   (T (com:win32-error ret :function-name 'directinput-create-device :type 'win32-error)))))))
    (ignore-errors ;; I don't know why we do this, but SDL2 seems to. I didn't notice any difference in capabilities.
     (cffi:with-foreign-object (dev2 :pointer)
       (check-return
        (device-query-interface dev IID-IDIRECTINPUTDEVICE8 dev2))
       (device-release dev)
       (setf dev (cffi:mem-ref dev2 :pointer))))
    (device-unacquire dev)
    (check-return
     (device-set-cooperative-level dev (device-notifier-window *device-notifier*) '(:background :exclusive)))
    ;; If we can't set the format then oh well.
    (device-set-data-format dev (joystate-format))
    (check-return
     (device-enum-objects dev (cffi:callback enum-objects) dev :axis))
    (let ((poll-device (eq :polled-device
                           (check-return
                            (device-set-event-notification dev *poll-event*) :ok :polled-device))))
      (unless poll-device
        ;; Allow receiving buffered events
        (cffi:with-foreign-object (dword '(:struct property-dword))
          (setf (property-dword-size dword) (cffi:foreign-type-size '(:struct property-dword)))
          (setf (property-dword-header-size dword) (cffi:foreign-type-size '(:struct property-header)))
          (setf (property-dword-how dword) :device)
          (setf (property-dword-type dword) 0)
          (setf (property-dword-data dword) EVENT-BUFFER-COUNT)
          (check-return
           (device-set-property dev DIPROP-BUFFERSIZE dword))))
      (cffi:with-foreign-object (instance '(:struct device-instance))
        (memset instance 0 (cffi:foreign-type-size '(:struct device-instance)))
        (setf (device-instance-size instance) (cffi:foreign-type-size '(:struct device-instance)))
        (check-return
         (device-get-device-info dev instance))
        (check-return
         (device-acquire dev))
        (let* ((product-guid (device-instance-product instance))
               (xinput (guess-xinput-id product-guid)))
          (make-instance 'device
                         :dev dev
                         :guid guid
                         :name (com:wstring->string (cffi:foreign-slot-pointer instance '(:struct device-instance) 'product-name))
                         :vendor (guid-vendor product-guid)
                         :product (guid-product product-guid)
                         :version 0
                         :poll-device poll-device
                         :effect (unless xinput (make-effect dev))
                         :xinput xinput
                         :driver (if xinput :xinput :dinput)))))))

(defun make-device-from-xinput (xinput)
  (let ((guid (com:guid (com:bytes IID-X360-WIRED-GAMEPAD))))
    (setf (aref (com:bytes guid) 4) xinput)
    (setf (gethash (com:guid-string guid) *device-table*)
          (make-instance 'device
                         :dev NIL :guid guid
                         :name "Xbox 360 Gamepad"
                         :vendor 0 :product 0 :version 0
                         :poll-device NIL :effect NIL
                         :xinput xinput :driver :xinput))))

(defun ensure-device (guid)
  (let ((guid-str (com:guid-string guid)))
    (or (gethash guid-str *device-table*)
        (with-simple-restart (drop-device "Don't initialise ~a" guid-str)
          (let ((device (make-device-from-guid guid)))
            (when device
              (setf (gethash guid-str *device-table*) device)))))))

(defun list-devices ()
  (loop for device being the hash-values of *device-table*
        collect device))

(defun call-with-devices (function)
  (loop for device being the hash-values of *device-table*
        do (funcall function device)))

(defun refresh-devices ()
  (let ((to-delete (list-devices)))
    (cffi:with-foreign-objects ((devices 'com:guid 256)
                                (enum-data '(:struct enum-user-data))
                                (xstate '(:struct xstate)))
      (setf (enum-user-data-directinput enum-data) *directinput*)
      (setf (enum-user-data-device-array enum-data) devices)
      (setf (enum-user-data-device-count enum-data) 0)
      (check-return
       (directinput-enum-devices *directinput* :game-controller (cffi:callback enum-devices) enum-data :attached-only))
      (loop for i from 0 below (enum-user-data-device-count enum-data)
            for device = (ensure-device (cffi:mem-aref devices 'com:guid i))
            do (setf to-delete (delete device to-delete)))
      ;; In case DirectInput fails completely we scan for Xbox controllers manually.
      (dolist (device (list-devices))
        (describe device))
      (loop for i from 0 below 4
            for device = (find i (list-devices) :key #'xinput)
            do (when (eq :ok (get-xstate i xstate))
                 (if device
                     (setf to-delete (delete device to-delete))
                     (make-device-from-xinput i))))
      (mapc #'close-device to-delete)
      (setf *devices-need-refreshing* NIL)
      (list-devices))))

(defun init ()
  (unless (boundp '*poll-event*)
    (com:init)
    (cffi:use-foreign-library user32)
    (setf *poll-event* (create-event (cffi:null-pointer) NIL NIL (com:string->wstring "ClGamepadPollEvent"))))
  (unless (boundp '*directinput*)
    (cffi:use-foreign-library xinput)
    (cffi:use-foreign-library dinput)
    (setf *directinput* (init-dinput)))
  (unless (boundp '*device-notifier*)
    (setf *device-notifier* (init-device-notifications)))
  (refresh-devices))

(defun shutdown ()
  (fill *xinput-taken* 0)
  (when (boundp '*device-notifier*)
    (unregister-device-notification (device-notifier-notification *device-notifier*))
    (destroy-window (device-notifier-window *device-notifier*))
    (unregister-class (device-notifier-class *device-notifier*) (get-module-handle (cffi:null-pointer)))
    (makunbound '*device-notifier*))
  (when (boundp '*directinput*)
    (mapc #'close-device (list-devices))
    (com:release *directinput*)
    (makunbound '*directinput*))
  (when (boundp '*poll-event*)
    (close-handle *poll-event*)
    (makunbound '*poll-event*)
    (com:shutdown)
    T))

(defun init-dinput ()
  (cffi:with-foreign-object (directinput :pointer)
    (check-return
     (create-direct-input (get-module-handle (cffi:null-pointer)) DINPUT-VERSION IID-IDIRECTINPUT8
                          directinput (cffi:null-pointer)))
    (cffi:mem-ref directinput :pointer)))

(defun init-device-notifications ()
  (cffi:with-foreign-objects ((window '(:struct window-class))
                              (broadcast '(:struct broadcast-device-interface)))
    (memset window 0 (cffi:foreign-type-size '(:struct window-class)))
    (setf (window-class-size window) (cffi:foreign-type-size '(:struct window-class)))
    (setf (window-class-procedure window) (cffi:callback device-change))
    (setf (window-class-instance window) (get-module-handle (cffi:null-pointer)))
    (setf (window-class-class-name window) (com:string->wstring "cl-gamepad-messages"))
    (memset broadcast 0 (cffi:foreign-type-size '(:struct broadcast-device-interface)))
    (setf (broadcast-device-interface-size broadcast) (cffi:foreign-type-size '(:struct broadcast-device-interface)))
    (setf (broadcast-device-interface-device-type broadcast) :device-interface)
    (setf (broadcast-device-interface-guid broadcast) GUID-DEVINTERFACE-HID)
    
    (let ((class (cffi:make-pointer (register-class window))))
      (check-errno (not (cffi:null-pointer-p class)))
      (let ((window (create-window 0 (window-class-class-name window) (cffi:null-pointer)
                                   0 0 0 0 0 HWND-MESSAGE (cffi:null-pointer) (cffi:null-pointer) (cffi:null-pointer))))
        (check-errno (not (cffi:null-pointer-p window))
          (unregister-class class (get-module-handle (cffi:null-pointer))))
        (let ((notify (register-device-notification window broadcast 0)))
          (check-errno (not (cffi:null-pointer-p notify))
            (destroy-window window)
            (unregister-class class (get-module-handle (cffi:null-pointer))))
          (make-device-notifier class window notify))))))

(defun call-with-polling (function handle timeout)
  (let ((ms (etypecase timeout
              ((eql T) 1000)
              ((eql NIL) 0)
              ((real 0) (floor (* 1000 timeout))))))
    (loop (let ((result (wait-for-single-object handle ms)))
            (when (eq :failed result)
              (com:win32-error (com-cffi:get-last-error) :type 'win32-error))
            (funcall function)
            (if (eql T timeout)
                ;; This is required to get SBCL/etc to process interrupts.
                (finish-output)
                (return))))))

(defmacro with-polling ((handle timeout) &body body)
  `(call-with-polling (lambda () ,@body) ,handle ,timeout))

(defun poll-devices (&key timeout)
  (when (boundp '*device-notifier*)
    (let* ((ms (etypecase timeout
                 ((eql T) 1000)
                 ((eql NIL) 0)
                 ((real 0) (floor (* 1000 timeout)))))
           (window (device-notifier-window *device-notifier*))
           (timer (when timeout (set-timer window 0 ms (cffi:null-pointer)))))
      (unwind-protect
           (cffi:with-foreign-object (message '(:struct message))
             (flet ((process ()
                      (when (get-message message window 0 0)
                        (translate-message message)
                        (dispatch-message message))))
               (loop
                  ;; First block with the timer if we have one
                  (when timer (process))
                  ;; Then remove remaining messages if there are any
                  (loop while (peek-message message window 0 0 0)
                        do (process))
                  ;; If we got a HID message we can now refresh.
                  (when *devices-need-refreshing*
                    (refresh-devices))
                  (if (eql T timeout)
                      ;; This is required to get SBCL/etc to process interrupts.
                      (finish-output)
                      (return)))))
        (when timer (kill-timer window timer))))))

(defmacro check-dinput-device (dev form)
  (let ((value (gensym "value")))
    `(let ((,value (check-return ,form :ok :false :input-lost :not-acquired)))
       (case ,value
         ((:ok :false))
         ((:input-lost :not-acquired)
          (check-return (device-acquire ,dev) :ok :false)
          (check-return ,form :ok :false))
         (T
          (error 'win32-error :code ,value :function-name ',(first form)))))))

(defun poll-events (device function &key timeout)
  (let ((dev (dev device))
        (xinput (xinput device)))
    (with-device-failures (device)
      (cond (xinput
             (cffi:with-foreign-objects ((state '(:struct xstate)))
               (with-polling (*poll-event* timeout)
                 (check-return (get-xstate xinput state))
                 (loop with packet = -1
                       while (/= packet (xstate-packet state))
                       do (setf packet (xstate-packet state))
                          (process-xinput-state (cffi:foreign-slot-pointer state '(:struct xstate) 'gamepad) device function)))))
            ((poll-device-p device)
             (cffi:with-foreign-objects ((state '(:struct joystate)))
               (loop do (check-dinput-device dev (device-poll dev))
                        (check-dinput-device dev (device-get-device-state dev (cffi:foreign-type-size '(:struct joystate)) state))
                        (process-joystate state device function)
                        ;; Terrible, but the best we can do.
                        (when (eql T timeout) (sleep 0.001))
                     while (eql T timeout))))
            (T
             (cffi:with-foreign-objects ((state '(:struct object-data) EVENT-BUFFER-COUNT)
                                         (count 'dword))
               (check-dinput-device dev (device-poll dev))
               (with-polling (*poll-event* timeout)
                 (setf (cffi:mem-ref count 'dword) EVENT-BUFFER-COUNT)
                 (check-dinput-device dev (device-get-device-data dev (cffi:foreign-type-size '(:struct object-data)) state count 0))
                 (loop for i from 0 below (cffi:mem-ref count 'dword)
                       for data = (cffi:mem-aptr state '(:struct object-data) i)
                       do (process-object-data data device function)))))))))

(defun map-to-float (min value max)
  (- (* (/ (- value min) (float (- max min) 0f0)) 2f0) 1f0))

(defun pov-to-cartesian (value)
  (if (or (= 65535 value) (= -1 value))
      (values 0f0 0f0)
      (let ((rad (* PI (/ (- 90 (/ value 100)) 180))))
        (values (float (cos rad) 0f0)
                (float (sin rad) 0f0)))))

(defun process-object-data (state device function)
  (let ((offset (object-data-offset state))
        (time (object-data-timestamp state)))
    (cond
      ;; Axis / Slider
      ((< offset (cffi:foreign-slot-offset '(:struct joystate) 'pov))
       (let* ((code (/ offset (cffi:foreign-type-size 'long)))
              (label (gethash code (axis-map device))))
         (signal-axis-move function device time code label (axis-to-float label (object-data-data state) -32768 32767))))
      ;; POV (emulate as two axes)
      ((< offset (cffi:foreign-slot-offset '(:struct joystate) 'buttons))
       (let ((code (+ 8 (* 2 (/ (- offset (cffi:foreign-slot-offset '(:struct joystate) 'pov)) (cffi:foreign-type-size 'dword))))))
         (multiple-value-bind (x y) (pov-to-cartesian (object-data-data state))
           (signal-axis-move function device time code (gethash (+ 0 code) (axis-map device)) x)
           (signal-axis-move function device time code (gethash (+ 1 code) (axis-map device)) y))))
      ;; Button
      (T
       (let* ((code (/ (- offset (cffi:foreign-slot-offset '(:struct joystate) 'buttons)) (cffi:foreign-type-size 'byte)))
              (label (gethash code (button-map device))))
         (if (= 1 (ldb (cl:byte 1 7) (object-data-data state)))
             (signal-button-down function device time code label)
             (signal-button-up function device time code label)))))))

(defun process-joystate (state device function)
  (let ((button-state (private-button-state device))
        (axis-state (private-axis-state device))
        (button-map (button-map device))
        (axis-map (axis-map device))
        (time (get-internal-real-time)))
    (labels ((handle-axis (id label new-state)
               (unless (= new-state (aref axis-state id))
                 (setf (aref axis-state id) new-state)
                 (signal-axis-move function device time id label new-state)))
             (handle-pov (id new-state)
               (multiple-value-bind (x y) (pov-to-cartesian new-state)
                 (handle-axis (+ 32 (* id 2)) (gethash (+ 32 (* id 2)) axis-map) x)
                 (handle-axis (+ 33 (* id 2)) (gethash (+ 33 (* id 2)) axis-map) y)))
             (handle-button (id new-state)
               (unless (eql (< 0 (sbit button-state id))
                            (< 0 new-state))
                 (setf (sbit button-state id) (if (< 0 new-state) 1 0))
                 (if (< 0 new-state)
                     (signal-button-down function device time id (gethash id button-map))
                     (signal-button-up function device time id (gethash id button-map))))))
      (loop with ptr = (cffi:foreign-slot-pointer state '(:struct joystate) 'axis)
            for i from 0 below 32
            for value = (cffi:mem-aref ptr 'long i)
            for label = (gethash i axis-map)
            do (handle-axis i label (axis-to-float label value -32768 32767)))
      (loop with ptr = (cffi:foreign-slot-pointer state '(:struct joystate) 'pov)
            for i from 0 below 4
            do (handle-pov i (cffi:mem-aref ptr 'dword i)))
      (loop with ptr = (cffi:foreign-slot-pointer state '(:struct joystate) 'buttons)
            for i from 0 below 36
            do (handle-button i (cffi:mem-aref ptr 'byte i))))))

(defun process-xinput-state (state device function)
  (let ((xbutton-state (xgamepad-buttons state))
        (button-state (button-states device))
        (time (get-internal-real-time)))
    (flet ((handle-button (label id new-state)
             (unless (eql (< 0 (sbit button-state id)) new-state)
               (setf (sbit button-state id) (if new-state 1 0))
               (if new-state
                   (signal-button-down function device time id label)
                   (signal-button-up function device time id label))))
           (handle-axis (label id new-state)
             ;; signal already handles deduplication and recording.
             (signal-axis-move function device time id label new-state)))
      (loop for (label id mask) in (load-time-value
                                    (loop for label in '(:dpad-u :dpad-d :dpad-l :dpad-r :start :select :l3 :r3 :l1 :r1 :a :b :x :y)
                                          collect (list label
                                                        (label-id label)
                                                        (cffi:foreign-bitfield-value 'xbuttons label))))
            do (handle-button label id (< 0 (logand mask xbutton-state))))
      (handle-axis :l2 (label-id :l2) (/ (xgamepad-left-trigger state) 255f0))
      (handle-axis :r2 (label-id :r2) (/ (xgamepad-right-trigger state) 255f0))
      (handle-axis :l-h (label-id :l-h) (map-to-float -32768 (xgamepad-lx state) 32767))
      (handle-axis :l-v (label-id :l-v) (map-to-float -32768 (xgamepad-ly state) 32767))
      (handle-axis :r-h (label-id :r-h) (map-to-float -32768 (xgamepad-rx state) 32767))
      (handle-axis :r-v (label-id :r-v) (map-to-float -32768 (xgamepad-ry state) 32767)))))

(defun rumble (device strength &key (pan 0))
  (let ((strength (clamp 0 strength 1))
        (pan (clamp -1 pan +1)))
    (with-device-failures (device)
      (cond ((xinput device)
             ;; On the X360 "left" and "right" denote strong and weak motors respectively, not the actual side. Sigh
             (cffi:with-foreign-object (xvibration '(:struct xvibration))
               (let ((strength (* 65535 strength)))
                 (setf (xvibration-left xvibration)
                       (floor (* strength (/ (1- pan) -2))))
                 (setf (xvibration-right xvibration)
                       (floor (* strength (/ (1+ pan) +2)))))
               (set-xstate (xinput device) xvibration)))
            ((effect device)
             (cffi:with-foreign-objects ((ff-effect '(:struct ff-effect))
                                         (axes :long)
                                         (direction :long))
               (setf (cffi:mem-ref axes :long) 0)
               (setf (cffi:mem-ref direction :long) (floor (* pan 10000)))
               (setf (ff-effect-size ff-effect) (cffi:foreign-type-size '(:struct ff-effect)))
               (setf (ff-effect-flags ff-effect) '(:cartesian))
               (setf (ff-effect-gain ff-effect) (floor (* 10000 strength)))
               (setf (ff-effect-axe-count ff-effect) 1)
               (setf (ff-effect-axe-identifiers ff-effect) axes)
               (setf (ff-effect-axe-directions ff-effect) direction)
               (effect-set-parameters (effect device) ff-effect '(:direction :gain))))
            (T
             :unsupported)))))

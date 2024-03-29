(in-package #:org.shirakumo.fraf.gamepad.impl)

(cffi:define-foreign-library dinput
  (T (:default "dinput8")))

(defconstant DINPUT-VERSION #x0800)
(defvar DIPROP-BUFFERSIZE (cffi:make-pointer 1))
(defvar DIPROP-RANGE (cffi:make-pointer 4))
(defvar DIPROP-DEADZONE (cffi:make-pointer 5))
(com:define-guid IID-IDIRECTINPUT8
  #xBF798031 #x483A #x4DA2 #xAA #x99 #x5D #x64 #xED #x36 #x97 #x00)
(com:define-guid IID-IDIRECTINPUTDEVICE8
  #x54D41081 #xDC15 #x4833 #xA4 #x1B #x74 #x8F #x73 #xA3 #x81 #x79)
(com:define-guid IID-VALVE-STREAMING-GAMEPAD
  #x28DE11FF #x0000 #x0000 #x00 #x00 #x50 #x49 #x44 #x56 #x49 #x44)
(com:define-guid IID-X360-WIRED-GAMEPAD
  #x045E02A1 #x0000 #x0000 #x00 #x00 #x50 #x49 #x44 #x56 #x49 #x44)
(com:define-guid IID-X360-WIRELESS-GAMEPAD
  #x045E028E #x0000 #x0000 #x00 #x00 #x50 #x49 #x44 #x56 #x49 #x44)
(com:define-guid GUID-XAXIS
  #xA36D02E0 #xC9F3 #x11CF #xBF #xC7 #x44 #x45 #x53 #x54 #x00 #x00)
(com:define-guid GUID-YAXIS
  #xA36D02E1 #xC9F3 #x11CF #xBF #xC7 #x44 #x45 #x53 #x54 #x00 #x00)
(com:define-guid GUID-ZAXIS
  #xA36D02E2 #xC9F3 #x11CF #xBF #xC7 #x44 #x45 #x53 #x54 #x00 #x00)
(com:define-guid GUID-RXAXIS
  #xA36D02F4 #xC9F3 #x11CF #xBF #xC7 #x44 #x45 #x53 #x54 #x00 #x00)
(com:define-guid GUID-RYAXIS
  #xA36D02F5 #xC9F3 #x11CF #xBF #xC7 #x44 #x45 #x53 #x54 #x00 #x00)
(com:define-guid GUID-RZAXIS
  #xA36D02E3 #xC9F3 #x11CF #xBF #xC7 #x44 #x45 #x53 #x54 #x00 #x00)
(com:define-guid GUID-SLIDER
  #xA36D02E4 #xC9F3 #x11CF #xBF #xC7 #x44 #x45 #x53 #x54 #x00 #x00)
(com:define-guid GUID-BUTTON
  #xA36D02F0 #xC9F3 #x11CF #xBF #xC7 #x44 #x45 #x53 #x54 #x00 #x00)
(com:define-guid GUID-KEY
  #x55728220 #xD33C #x11CF #xBF #xC7 #x44 #x45 #x53 #x54 #x00 #x00)
(com:define-guid GUID-POV
  #xA36D02F2 #xC9F3 #x11CF #xBF #xC7 #x44 #x45 #x53 #x54 #x00 #x00)
(com:define-guid GUID-CONSTANT-FORCE
  #x13541C20 #x8E33 #x11D0 #x9A #xD0 #x00 #xA0 #xC9 #xA0 #x6E #x35)
(com:define-guid GUID-RAMP-FORCE
  #x13541C21 #x8E33 #x11D0 #x9A #xD0 #x00 #xA0 #xC9 #xA0 #x6E #x35)
(com:define-guid GUID-SQUARE
  #x13541C22 #x8E33 #x11D0 #x9A #xD0 #x00 #xA0 #xC9 #xA0 #x6E #x35)
(com:define-guid GUID-SINE
  #x13541C23 #x8E33 #x11D0 #x9A #xD0 #x00 #xA0 #xC9 #xA0 #x6E #x35)
(com:define-guid GUID-TRIANGLE
  #x13541C24 #x8E33 #x11D0 #x9A #xD0 #x00 #xA0 #xC9 #xA0 #x6E #x35)
(com:define-guid GUID-SAWTOOTH-UP
  #x13541C25 #x8E33 #x11D0 #x9A #xD0 #x00 #xA0 #xC9 #xA0 #x6E #x35)
(com:define-guid GUID-SAWTOOTH-DOWN
  #x13541C26 #x8E33 #x11D0 #x9A #xD0 #x00 #xA0 #xC9 #xA0 #x6E #x35)
(com:define-guid GUID-SPRING
  #x13541C27 #x8E33 #x11D0 #x9A #xD0 #x00 #xA0 #xC9 #xA0 #x6E #x35)
(com:define-guid GUID-DAMPER
  #x13541C28 #x8E33 #x11D0 #x9A #xD0 #x00 #xA0 #xC9 #xA0 #x6E #x35)
(com:define-guid GUID-INERTIA
  #x13541C29 #x8E33 #x11D0 #x9A #xD0 #x00 #xA0 #xC9 #xA0 #x6E #x35)
(com:define-guid GUID-FRICTION
  #x13541C2A #x8E33 #x11D0 #x9A #xD0 #x00 #xA0 #xC9 #xA0 #x6E #x35)
(com:define-guid GUID-CUSTOM-FORCE
  #x13541C2B #x8E33 #x11D0 #x9A #xD0 #x00 #xA0 #xC9 #xA0 #x6E #x35)

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
  (:any-instance  #x00FFFF00)
  (:ff-actuator   #x01000000)
  (:ff-trigger    #x02000000)
  (:optional      #x80000000))

(cffi:defbitfield (cooperation-flags dword)
  (:exclusive    #x01)
  (:nonexclusive #x02)
  (:foreground   #x04)
  (:background   #x08)
  (:no-win-key   #x10))

(cffi:defcenum (enumerate-flag word)
  (:stop 0)
  (:continue 1))

(cffi:defcenum (property-header-flag dword)
  (:device 0)
  (:by-offset 1)
  (:by-id 2)
  (:by-usage 3))

(cffi:defbitfield (effect-flags dword)
  (:object-ids     #x00000001)
  (:object-offsets #x00000002)
  (:cartesian      #x00000010)
  (:polar          #x00000020)
  (:spherical      #x00000040))

(cffi:defbitfield (effect-status dword)
  (:playing #x1)
  (:emulated #x2))

(cffi:defbitfield (effect-parameter-types dword)
  (:duration             #x00000001)
  (:sample-period        #x00000002)
  (:gain                 #x00000004)
  (:trigger-button       #x00000008)
  (:trigger-repeat-interval #x00000010)
  (:axes                 #x00000020)
  (:direction            #x00000040)
  (:envelope             #x00000080)
  (:type-specific-params #x00000100)
  (:start-delay          #x00000200)
  (:all-params-dx5       #x000001ff)
  (:all-params           #x000003ff))

(cffi:defbitfield (effect-start-flags dword)
  (:solo        #x00000001)
  (:no-download #x80000000))

(cffi:defbitfield (effect-types dword)
  (:all	                 #x00000000)
  (:constant-force       #x00000001)
  (:ramp-force           #x00000002)
  (:periodic             #x00000003)
  (:condition            #x00000004)
  (:custom-force         #x00000005)
  (:hardware             #x000000FF)
  (:ff-attack            #x00000200)
  (:ff-fade              #x00000400)
  (:saturation           #x00000800)
  (:pos-neg-coefficients #x00001000)
  (:pos-neg-saturation   #x00002000)
  (:deadband             #x00004000)
  (:start-delay          #x00008000))

(cffi:defcstruct (device-instance :conc-name device-instance-)
  (size dword)
  (guid com:guid)
  (product com:guid)
  (type dword)
  (instance-name wchar :count #.MAX-PATH)
  (product-name wchar :count #.MAX-PATH)
  (ff-driver com:guid)
  (usage-page word)
  (usage word))

(cffi:defcstruct (enum-user-data :conc-name enum-user-data-)
  (directinput :pointer)
  (device-array :pointer)
  (device-count :uint8))

(cffi:defcstruct (object-data-format :conc-name object-data-format-)
  (guid :pointer)
  (offset dword)
  (type dword)
  (flags dword))

(cffi:defcstruct (data-format :conc-name data-format-)
  (size dword)
  (object-size dword)
  (flags dword)
  (data-size dword)
  (object-count dword)
  (object-data-format :pointer))

(cffi:defcstruct (object-data :conc-name object-data-)
  (offset dword)
  (data :int32)
  (timestamp dword)
  (sequence dword)
  (app-data :pointer))

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
  (guid com:guid)
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

(cffi:defcstruct (broadcast-device-interface :conc-name broadcast-device-interface-)
  (size dword)
  (device-type win-device-type)
  (reserved dword)
  (guid com:guid)
  (name wchar))

(cffi:defcstruct (ff-envelope :conc-name ff-envelope-)
  (size dword)
  (attack-level dword)
  (attack-time dword)
  (fade-level dword)
  (fade-time dword))

(cffi:defcstruct (ff-condition :conc-name ff-condition-)
  (offset :long)
  (positive-coefficient :long)
  (negative-coefficient :long)
  (positive-saturation dword)
  (negative-saturation dword)
  (dead-band :long))

(cffi:defcstruct (ff-custom :conc-name ff-custom-)
  (channels dword)
  (sample-period dword)
  (samples dword)
  (force-data :pointer))

(cffi:defcstruct (ff-periodic :conc-name ff-periodic-)
  (magnitude dword)
  (offset :long)
  (phase dword)
  (period dword))

(cffi:defcstruct (ff-constant :conc-name ff-constant-)
  (magnitude :long))

(cffi:defcstruct (ff-ramp :conc-name ff-ramp-)
  (start :long)
  (end :long))

(cffi:defcstruct (ff-effect :conc-name ff-effect-)
  (size dword)
  (flags effect-flags)
  (duration dword)
  (sample-period dword)
  (gain dword)
  (trigger-button dword)
  (trigger-repeat-interval dword)
  (axe-count dword)
  (axe-identifiers :pointer)
  (axe-directions :pointer)
  (envelope :pointer)
  (specific-size dword)
  (specific :pointer)
  (start-delay dword))

(cffi:defcstruct (effect-info :conc-name effect-info-)
  (size dword)
  (guid com:guid)
  (type effect-types)
  (static dword)
  (dynamic dword)
  (name wchar :count #.MAX-PATH))

(cffi:defcfun (create-direct-input "DirectInput8Create") com:hresult
  (instance :pointer)
  (version dword)
  (refiid com:guid)
  (interface :pointer)
  (aggregation :pointer))

(com:define-comstruct directinput
  (create-device (guid com:guid) (device :pointer) (outer :pointer))
  (enum-devices (type device-type) (callback :pointer) (user :pointer) (flags device-flags))
  (get-device-status (instance :pointer))
  (run-control-panel (owner :pointer) (flags dword))
  (initialize (instance :pointer) (version dword))
  (find-device (guid com:guid) (name :pointer) (instance :pointer))
  (enum-devices-by-semantics (user-name :pointer) (action-format :pointer) (callback :pointer) (user :pointer) (flags dword))
  (configure-devices (callback :pointer) (params :pointer) (flags dword) (user :pointer)))

(com:define-comstruct device
  (get-capabilities (caps :pointer))
  (enum-objects (callback :pointer) (user :pointer) (flags object-flags))
  (get-property (property :pointer) (header :pointer))
  (set-property (property :pointer) (header :pointer))
  (acquire)
  (unacquire)
  (get-device-state (data dword) (data* :pointer))
  (get-device-data (object-data dword) (object-data* :pointer) (inout :pointer) (flags dword))
  (set-data-format (format :pointer))
  (set-event-notification (event :pointer))
  (set-cooperative-level (hwnd :pointer) (flags cooperation-flags))
  (get-object-info (instance :pointer) (object dword) (how dword))
  (get-device-info (instance :pointer))
  (run-control-panel (owner :pointer) (flags dword))
  (initialize (instance :pointer) (version dword) (guid com:guid))
  (create-effect (guid com:guid) (effect :pointer) (input-effect :pointer) (user :pointer))
  (enum-effects (callback :pointer) (user :pointer) (type effect-types))
  (get-effect-info (info :pointer) (guid com:guid))
  (get-force-feedback-state (out :pointer))
  (send-force-feedback-command (flags dword))
  (enum-created-effect-objects (callback :pointer) (user :pointer) (flags dword))
  (escape (escape :pointer))
  (poll)
  (send-device-data (object-data dword) (object-data* :pointer) (inout :pointer) (flags dword))
  (enum-effects-in-file (file-name :pointer) (callback :pointer) (user :pointer) (flags dword))
  (write-effect-to-file (file-name :pointer) (entries dword) (effects :pointer) (flags dword))
  (bild-action-map (format :pointer) (user-name :pointer) (flags dword))
  (set-action-map (format :pointer) (user-name :pointer) (flags dword))
  (get-image-info (image-info :pointer)))

(com:define-comstruct effect
  (initialize (hinstance :pointer) (version dword) (guid com:guid))
  (get-effect-guid (guid com:guid))
  (get-parameters (ff-effect :pointer) (flags effect-parameter-types))
  (set-parameters (ff-effect :pointer) (flags effect-parameter-types))
  (start (times dword) (flags effect-start-flags))
  (stop)
  (get-effect-status (status :pointer))
  (download)
  (unload)
  (escape (escape :pointer)))

;;; Construct our own joystate
(cffi:defcstruct (joystate :conc-name joystate-)
  (axis long :count 32)
  (pov dword :count 4)
  (buttons byte :count 36))

(defvar *joystate-format* NIL)

(defun make-joystate-format ()
  (let ((fields (cffi:foreign-alloc '(:struct object-data-format) :count 72))
        (format (cffi:foreign-alloc '(:struct data-format)))
        (i 0) (offset 0))
    (flet ((set-field (guid type size)
             (let ((ptr (cffi:mem-aptr fields '(:struct object-data-format) i))
                   (guid (cffi:translate-to-foreign guid 'com:guid)))
               (setf (object-data-format-guid ptr) guid
                     (object-data-format-offset ptr) offset
                     (object-data-format-type ptr) (cffi:foreign-bitfield-value 'object-flags (list type :optional :any-instance))
                     (object-data-format-flags ptr) 0)
               (incf i)
               (incf offset size))))
      (dotimes (_ 4)
        (loop for guid in (list GUID-XAXIS GUID-YAXIS GUID-ZAXIS GUID-RXAXIS GUID-RYAXIS GUID-RZAXIS GUID-SLIDER GUID-SLIDER)
              do (set-field guid :absolute-axis (cffi:foreign-type-size 'long))))
      (dotimes (_ 4)
        (set-field GUID-POV :pov (cffi:foreign-type-size 'dword)))
      (dotimes (_ 36)
        (set-field (cffi:null-pointer) :button (cffi:foreign-type-size 'byte))))
    (setf (data-format-size format) (cffi:foreign-type-size '(:struct data-format))
          (data-format-object-size format) (cffi:foreign-type-size '(:struct object-data-format))
          (data-format-flags format) 1
          (data-format-data-size format) (cffi:foreign-type-size '(:struct joystate))
          (data-format-object-count format) 72
          (data-format-object-data-format format) fields)
    format))

(defun joystate-format ()
  (or *joystate-format*
      (setf *joystate-format* (make-joystate-format))))

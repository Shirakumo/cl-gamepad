(in-package #:org.shirakumo.fraf.gamepad.impl)

(cffi:define-foreign-library evdev
  (T (:or "libevdev.so.2" "libevdev.so.1" "libevdev.so" "libevdev-lin-amd64.so")))

(cffi:defctype fd :int)
(cffi:defctype errno :int64)

(cffi:defbitfield open-flag
  (:read          #o0000000)
  (:write         #o0000002)
  (:create        #o0000100)
  (:ensure-create #o0000200)
  (:dont-claim-tty#o0000400)
  (:truncate      #o0001000)
  (:non-block     #o0004000)
  (:data-sync     #o0010000)
  (:async         #o0020000)
  (:direct        #o0040000)
  (:large-file    #o0100000)
  (:directory     #o0200000)
  (:no-follow     #o0400000)
  (:file-sync     #o4010000))

(cffi:defbitfield read-flag
  (:sync       1)
  (:normal     2)
  (:force-sync 4)
  (:blocking   8))

(cffi:defcenum read-status
  (:success 0)
  (:sync    1)
  (:bad-fd -9)
  (:again -11)
  (:out-of-memory -12)
  (:device-busy -16)
  (:no-such-device -19))

(cffi:defcenum property
  (:pointer        #x0)
  (:direct         #x1)
  (:button-pad     #x2)
  (:sim-mt         #x3)
  (:top-button-pad #x4)
  (:pointing-stick #x5)
  (:accelerometer  #x6))

(cffi:defcenum (event-type :uint16 :allow-undeclared-values T)
  (:synchronization #x00)
  (:key             #x01)
  (:relative-axis   #x02)
  (:absolute-axis   #x03)
  (:miscellaneous   #x04)
  (:switch          #x05)
  (:led             #x11)
  (:sound           #x12)
  (:repeat          #x14)
  (:force-feedback  #x15)
  (:power           #x16)
  (:force-feedback-status #x17))

(cffi:defcenum (effect-status :uint16)
  (:stopped #x00)
  (:playing #x01))

(cffi:defcenum (effect-type :uint16)
  (:rumble   #x50)
  (:periodic #x51)
  (:constant #x52)
  (:spring   #x53)
  (:friction #x54)
  (:damper   #x55)
  (:inertia  #x56)
  (:ramp     #x57))

(cffi:defcenum (periodic-effect-type :uint16)
  (:square   #x58)
  (:triangle #x59)
  (:sine     #x5A)
  (:saw-up   #x5B)
  (:saw-down #x5C)
  (:custom   #x5D))

(cffi:defcenum (effect-direction :uint16)
  (:down  #x0000)
  (:left  #x4000)
  (:up    #x8000)
  (:right #xC000))

(cffi:defcenum (ioctl :uint)
  (:send-effect         #x40304580)
  (:remove-effect       #x40044581)
  (:effect-count        #x80044584)
  (:effect-capabilities #x80024535))

(cffi:defbitfield (poll-event :short)
  (:in  #x001)
  (:pri #x002)
  (:out #x004))

(cffi:defbitfield (inotify-event :uint32)
  (:create #x00000100)
  (:delete #x00000200))

(cffi:defbitfield inotify-flag
  (:nonblock #o0004000)
  (:cloexec  #o2000000))

(cffi:defcstruct (axis-info :conc-name axis-info-)
  (value :int32)
  (minimum :int32)
  (maximum :int32)
  (fuzz :int32)
  (flat :int32)
  (resolution :int32))

(cffi:defcstruct (event :conc-name event-)
  (sec :uint64)
  (usec :uint64)
  (type event-type)
  (code :uint16)
  (value :int32))

(cffi:defcstruct (pollfd :conc-name pollfd-)
  (fd fd)
  (events poll-event)
  (revents poll-event))

(cffi:defcstruct (inotify :conc-name inotify-)
  (wd :int)
  (mask inotify-event)
  (cookie :uint32)
  (length :uint32)
  (name :char :count 0))

(cffi:defcstruct (ff-replay :conc-name ff-replay-)
  (length :uint16)
  (delay :uint16))

(cffi:defcstruct (ff-trigger :conc-name ff-trigger-)
  (button :uint16)
  (interval :uint16))

(cffi:defcstruct (ff-envelope :conc-name ff-envelope-)
  (attack-length :uint16)
  (attack-level :uint16)
  (fade-length :uint16)
  (fade-level :uint16))

(cffi:defcstruct (ff-constant :conc-name ff-constant-)
  (level :int16)
  (envelope (:struct ff-envelope)))

(cffi:defcstruct (ff-ramp :conc-name ff-ramp-)
  (start-level :int16)
  (end-level :int16)
  (envelope (:struct ff-envelope)))

(cffi:defcstruct (ff-condition :conc-name ff-condition-)
  (right-saturation :uint16)
  (left-saturation :uint16)
  (right-coefficient :int16)
  (left-coefficient :int16)
  (deadband :uint16)
  (center :int16))

(cffi:defcstruct (ff-periodic :conc-name ff-periodic-)
  (waveform periodic-effect-type)
  (period :uint16)
  (magnitude :int16)
  (offset :int16)
  (phase :uint16)
  (envelope (:struct ff-envelope))
  (custom-length :uint32)
  (custom-data :pointer))

(cffi:defcstruct (ff-rumble :conc-name ff-rumble-)
  (strong-magnitude :uint16)
  (weak-magnitude :uint16))

(cffi:defcunion ff-effect
  (constant (:struct ff-constant))
  (ramp (:struct ff-ramp))
  (periodic (:struct ff-periodic))
  (condition (:struct ff-condition) :count 2)
  (rumble (:struct ff-rumble)))

(cffi:defcstruct (effect :conc-name effect-)
  (type effect-type)
  (id :uint16)
  (direction effect-direction)
  (trigger (:struct ff-trigger))
  (replay (:struct ff-replay))
  (data (:union ff-effect)))

(cffi:defcfun (u-open "open") fd
  (pathname :string)
  (mode open-flag))

(cffi:defcfun (u-close "close") :int
  (fd fd))

(cffi:defcfun (u-read "read") :int
  (fd fd)
  (buffer :pointer)
  (length :int))

(cffi:defcfun (u-write "write") :int
  (fd fd)
  (buffer :pointer)
  (length :int))

(cffi:defcfun (poll "poll") :int
  (pollfds :pointer)
  (n :int)
  (timeout :int))

(cffi:defcfun (memset "memset") :pointer
  (pointer :pointer)
  (fill :int)
  (n :uint))

(cffi:defcfun (new-inotify "inotify_init1") fd
  (flags inotify-flag))

(cffi:defcfun (add-watch "inotify_add_watch") errno
  (fd fd)
  (path :string)
  (mask inotify-event))

(cffi:defcfun (new-from-fd "libevdev_new_from_fd") errno
  (fd fd)
  (device :pointer))

(cffi:defcfun (change-fd "libevdev_change_fd") errno
  (device :pointer)
  (fd fd))

(cffi:defcfun (free-device "libevdev_free") :void
  (device :pointer))

(cffi:defcfun (get-name "libevdev_get_name") :string
  (device :pointer))

(cffi:defcfun (get-uniq "libevdev_get_uniq") :string
  (device :pointer))

(cffi:defcfun (get-id-bustype "libevdev_get_id_bustype") :int
  (device :pointer))

(cffi:defcfun (get-id-vendor "libevdev_get_id_vendor") :int
  (device :pointer))

(cffi:defcfun (get-id-product "libevdev_get_id_product") :int
  (device :pointer))

(cffi:defcfun (get-id-version "libevdev_get_id_version") :int
  (device :pointer))

(cffi:defcfun (get-driver-version "libevdev_get_driver_version") :int
  (device :pointer))

(cffi:defcfun (has-event-code "libevdev_has_event_code") :boolean
  (device :pointer)
  (type event-type)
  (code :uint))

(cffi:defcfun (has-event-type "libevdev_has_event_type") :boolean
  (device :pointer)
  (type event-type))

(cffi:defcfun (has-property "libevdev_has_property") :boolean
  (device :pointer)
  (property property))

(cffi:defcfun (get-axis-info "libevdev_get_abs_info") :pointer
  (device :pointer)
  (code :uint))

(cffi:defcfun (has-event-pending "libevdev_has_event_pending") errno
  (device :pointer))

(cffi:defcfun (next-event "libevdev_next_event") read-status
  (device :pointer)
  (flag read-flag)
  (event :pointer))

(cffi:defcfun (ioctl "ioctl") errno
  (fd fd)
  (request ioctl)
  (data :pointer))

(cffi:defcfun (error-message "strerror") :string
  (errno errno))

(cffi:defcvar (errno "errno") errno)

(define-condition linux-error (gamepad:gamepad-error)
  ((function-name :initarg :function-name :initform NIL :reader function-name)
   (code :initarg :code :reader code)
   (message :initarg :message :initform NIL :reader message))
  (:report (lambda (c s) (format s "The call ~@[to~%  ~a~%~]returned with unexpected result code ~a.~@[~%  ~a~]"
                                 (function-name c) (code c) (message c)))))

(declaim (inline linux-error))
(defun linux-error (code &key function-name message)
  (error 'linux-error :code code :function-name function-name
                      :message (or message (error-message code))))

(defmacro check-errno (predicate &body case-forms)
  `(unless ,predicate
     (let ((code errno))
       (case code
         ,@case-forms
         (T (linux-error code))))))

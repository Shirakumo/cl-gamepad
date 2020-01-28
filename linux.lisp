#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(in-package #:org.shirakumo.fraf.gamepad.impl)

(cffi:define-foreign-library evdev
  (T (:default "libevdev")))

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

(cffi:defcenum read-flag
  (:sync       1)
  (:normal     2)
  (:force-sync 4)
  (:blocking   8))

(cffi:defcenum property
  (:pointer        #x0)
  (:direct         #x1)
  (:button-pad     #x2)
  (:sim-mt         #x3)
  (:top-button-pad #x4)
  (:pointing-stick #x5)
  (:accelerometer  #x6))

(cffi:defcenum event-type
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

(cffi:defcstruct (event :conc-name event)
  (sec :uint32)
  (usec :uint32)
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

(cffi:defcfun (u-open "open") fd
  (pathname :string)
  (mode open-flag))

(cffi:defcfun (u-close "close") :int
  (fd fd))

(cffi:defcfun (u-read "read") :int
  (fd fd)
  (buffer :pointer)
  (length :int))

(cffi:defcfun (poll "poll") :int
  (pollfds :pointer)
  (n :int)
  (timeout :int))

(cffi:defcfun (new-inotify "inotify_init1") fd
  (flags inotify-flag))

(cffi:defcfun (add-watch "inotify_add_watch") errno
  (fd fd)
  (path :string)
  (mask inotify-event))

(cffi:defcfun (new-from-fd "libevdev_new_from_fd") errno
  (fd fd)
  (device :pointer))

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

(cffi:defcfun (next-event "libevdev_next_event") errno
  (device :pointer)
  (flag read-flag)
  (event :pointer))

(defvar *device-table* (make-hash-table :test 'eql))

(defclass device (gamepad::device)
  ((id :initarg :id :reader id)
   (fd :initarg :fd :reader fd)
   (dev :initarg :dev :reader dev)))

(defun device-button-map (dev)
  (let ((map (make-hash-table :test 'eql :size (length gamepad::*labels*))))
    (loop for label across gamepad::*labels*
          for id across #(#x130 #x131 #x132
                          #x133 #x134 #x135
                          #x136 #x137 #x13D
                          #x138 #x139 #x13E
                          #x222 #x223 #x220 #x221
                          #x13A #x13C #x13B)
          do (when (and id (has-event-code dev :key id))
               (setf (gethash id map) label)))
    map))

(defun device-axis-map (dev)
  (let ((map (make-hash-table :test 'eql :size (length gamepad::*labels*))))
    (loop for label across gamepad::*labels*
          for id across #(NIL NIL NIL
                          NIL NIL NIL
                          #x13 #x15 NIL
                          #x12 #x14 NIL
                          NIL NIL NIL NIL
                          NIL NIL NIL
                          #x00 #x01 #x03 #x04
                          #x10 #x11
                          #x1A #x1B NIL
                          NIL NIL NIL
                          #x08 #x09 #x0A #x06 #x07)
          do (when (and id (has-event-code dev :absolute-axis id))
               (setf (gethash id map) label)))
    map))

(defun make-device-from-path (path)
  (let ((fd (u-open (namestring path)
                    (load-time-value
                     (logior (cffi:foreign-enum-value 'open-flag :read)
                             (cffi:foreign-enum-value 'open-flag :write)
                             (cffi:foreign-enum-value 'open-flag :non-block))))))
    (assert (<= 0 fd))
    (cffi:with-foreign-object (dev :pointer)
      (assert (<= 0 (new-from-fd fd dev)))
      (let ((dev (cffi:mem-ref dev :pointer)))
        (make-instance 'device :id (parse-integer (subseq path (length "/dev/input/event")))
                               :fd fd
                               :dev dev
                               :name (get-name dev)
                               :vendor (get-id-vendor dev)
                               :product (get-id-product dev)
                               :version (get-id-version dev)
                               :driver-version (get-driver-version dev)
                               :button-map (device-button-map dev)
                               :axis-map (device-axis-map dev))))))

(defun close-device (device)
  (free-device (dev device))
  (u-close (fd device))
  (slot-makunbound device 'dev)
  (slot-makunbound device 'fd)
  (remhash (id device) *device-table*))

(defun ensure-device (path)
  (let* ((path (namestring path))
         (id (parse-integer (subseq path (length "/dev/input/event"))))
         (device (gethash id *device-table*)))
    (unless device
      (setf device (make-device-from-path path))
      (setf (gethash id *device-table*) device))
    device))

(defmacro with-protection ((binding value &optional check) cleanup &body body)
  `(let ((,binding ,value))
     ,@(when check `((assert ,check)))
     (unwind-protect
          (progn ,@body)
       ,cleanup)))

(defun dev-gamepad-p (device)
  (and (has-event-type device :key)
       (has-event-type device :absolute-axis)
       (has-event-code device :absolute-axis #x00)
       (has-event-code device :absolute-axis #x01)
       (or
        (has-event-code device :key #x120)
        (has-event-code device :key #x130)
        (has-event-code device :key #x101))))

(defun list-gamepads ()
  (loop for device being the hash-values of *device-table*
        collect device))

(defun refresh-gamepads ()
  (mapcar #'close-device (list-gamepads))
  (dolist (path (directory "/dev/input/event*") (list-gamepads))
    (let ((device (make-device-from-path (namestring path))))
      (if (dev-gamepad-p (dev device))
          (setf (gethash (id device) *device-table*) device)
          (close-device device)))))

(defun prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun call-with-device-updates (function &key (timeout 1))
  (with-protection (inotify (new-inotify :nonblock) (<= 0 inotify))
                   (u-close inotify)
    (add-watch inotify "/dev/input" '(:create :delete))
    (cffi:with-foreign-objects ((buffer '(:struct inotify) 1024)
                                (pollfd '(:struct pollfd)))
      (setf (pollfd-fd pollfd) inotify)
      (setf (pollfd-events pollfd) '(:in :pri))
      (setf (pollfd-revents pollfd) 0)
      (loop for poll = (poll pollfd 1 (floor (* timeout 1000)))
            for read = (case poll
                         (1 (u-read inotify buffer (* 1024 (cffi:foreign-type-size '(:struct inotify)))))
                         (0 0)
                         (T (error "Poll failed.")))
            do (when (< 0 read)
                 (loop with i = 0
                       while (< i read)
                       do (let* ((struct (cffi:inc-pointer buffer i))
                                 (action (first (inotify-mask struct)))
                                 (path (cffi:foreign-string-to-lisp (cffi:foreign-slot-pointer struct '(:struct inotify) 'name)
                                                                    :max-chars (inotify-length struct))))
                            (incf i (+ (inotify-length struct) (cffi:foreign-type-size '(:struct inotify))))
                            (when (prefix-p "event" path)
                              (funcall function (format NIL "/dev/input/~a" path) action)))))))))

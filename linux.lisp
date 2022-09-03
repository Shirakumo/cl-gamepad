#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.impl)

(defvar *device-table* (make-hash-table :test 'eql))
(defvar *device-notify* NIL)

(defun device-button-map (dev)
  (let ((map (make-hash-table :test 'eql :size (length +labels+))))
    (loop for label across +labels+
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
  (let ((map (make-hash-table :test 'eql :size (length +labels+)))
        (orientation (make-hash-table :test 'eql :size (length +labels+))))
    (loop for label across +labels+
          for id across #(NIL NIL NIL
                          NIL NIL NIL
                          #x13 (#x15 #x02) NIL
                          #x12 (#x14 #x05) NIL
                          NIL NIL NIL NIL
                          NIL NIL NIL
                          #x00 #x01 #x03 #x04
                          #x10 #x11
                          #x1A #x1B NIL
                          NIL NIL NIL
                          #x08 #x09 #x0A #x06 #x07)
          do (dolist (id (if (listp id) id (list id)))
               (when (and id (has-event-code dev :absolute-axis id))
                 (setf (gethash id orientation) (if (find label '(:l-v :r-v :dpad-v)) -1.0 1.0))
                 (setf (gethash id map) label))))
    (values map orientation)))

;; See /usr/include/linux/input-event-codes.h
(defun dev-gamepad-p (dev)
  (flet ((gamepad-p ()
           (and
            ;; Check if it at least has A / B
            (has-event-code dev :key #x130)
            (has-event-code dev :key #x131)
            ;; Check if it at least has a digital or analog DPAD or an axis
            (or (and (has-event-code dev :key #x222)
                     (has-event-code dev :key #x223)
                     (has-event-code dev :key #x220)
                     (has-event-code dev :key #x221))
                (and (has-event-type dev :absolute-axis)
                     (has-event-code dev :absolute-axis #x10)
                     (has-event-code dev :absolute-axis #x11))
                (and (has-event-type dev :absolute-axis)
                     (has-event-code dev :absolute-axis #x00)
                     (has-event-code dev :absolute-axis #x01)))))
         (joystick-p ()
           (and
            (has-event-code dev :key #x120) ; trigger
            (has-event-code dev :key #x121) ; thumb
            (has-event-type dev :absolute-axis)
            (and (has-event-code dev :absolute-axis #x00)
                 (has-event-code dev :absolute-axis #x01)))))
    ;; KLUDGE: Heuristics
    (and (has-event-type dev :key)
         (or (gamepad-p)
             (joystick-p)))))

(defun probe-device-effect (fd)
  ;; We want to create an effect that lasts up to a tenth of a second of constant volume.
  ;; Unfortunately devices lie to us about what they support, so we have to
  ;; manually try and upload effects until we get one that works and approximates
  ;; what we want.
  ;;
  ;; In my brief testing devices simply did not support any complicated effect schemes
  ;; no matter what they actually accept or advertise. Meaning that the effect I uploaded
  ;; simply did not behave as it is described to.
  ;;
  ;; We are therefore forced to handle rumbling strength schemes in user code, and only
  ;; expose the minimal viable interface to the users, namely strength of rumbling.
  (let ((effect (cffi:foreign-alloc '(:struct effect))))
    (memset effect 0 (cffi:foreign-type-size '(:struct effect)))
    (let ((replay (cffi:foreign-slot-pointer effect '(:struct effect) 'replay))
          (data (cffi:foreign-slot-pointer effect '(:struct effect) 'data)))
      (setf (effect-id effect) 65535)
      (setf (effect-direction effect) :up)
      (setf (ff-replay-length replay) 100)
      (block NIL
        (setf (effect-type effect) :constant)
        (setf (ff-constant-level data) #x7FFF)
        (when (<= 0 (ioctl fd :send-effect effect))
          (return effect))

        (setf (effect-type effect) :rumble)
        (setf (ff-rumble-strong-magnitude data) #x7FFF)
        (setf (ff-rumble-weak-magnitude data) #x7FFF)
        (when (<= 0 (ioctl fd :send-effect effect))
          (return effect))
        
        (setf (effect-type effect) :periodic)
        (setf (ff-periodic-waveform data) :sine)
        (setf (ff-periodic-period data) 1)
        (setf (ff-periodic-magnitude data) #x7FFF)
        (setf (ff-periodic-offset data) 0)
        (setf (ff-periodic-phase data) 0)
        (when (<= 0 (ioctl fd :send-effect effect))
          (return effect))
        
        (cffi:foreign-free effect)
        NIL))))

(defclass device (gamepad:device)
  ((id :initarg :id :reader id)
   (fd :initarg :fd :reader fd)
   (dev :initarg :dev :reader dev)
   (effect :initarg :effect :reader effect)))

(defun make-device-from-path (path)
  (let ((fd (u-open (namestring path)
                    (load-time-value
                     (logior (cffi:foreign-enum-value 'open-flag :read)
                             (cffi:foreign-enum-value 'open-flag :write)
                             (cffi:foreign-enum-value 'open-flag :non-block))))))
    (when (<= 0 fd)
      (cffi:with-foreign-object (dev :pointer)
        (assert (<= 0 (new-from-fd fd dev)))
        (let ((dev (cffi:mem-ref dev :pointer)))
          (multiple-value-bind (axis-map orientation-map) (device-axis-map dev)
            (make-instance 'device :id (parse-integer (subseq path (length "/dev/input/event")))
                                   :fd fd
                                   :dev dev
                                   :name (get-name dev)
                                   :vendor (get-id-vendor dev)
                                   :product (get-id-product dev)
                                   :version (get-id-version dev)
                                   :driver :evdev
                                   :effect (probe-device-effect fd)
                                   :button-map (device-button-map dev)
                                   :axis-map axis-map
                                   :orientation-map orientation-map)))))))

(defun close-device (device)
  (when (slot-boundp device 'dev)
    (free-device (dev device)))
  (when (effect device)
    (ioctl (fd device) :remove-effect (cffi:make-pointer (effect-id (effect device))))
    (cffi:foreign-free (effect device))
    (setf (slot-value device 'effect) NIL)
    (when (slot-boundp device 'fd)
      (u-close (fd device))))
  (slot-makunbound device 'dev)
  (slot-makunbound device 'fd)
  (remhash (id device) *device-table*))

(defun ensure-device (path)
  (let* ((path (namestring path))
         (id (parse-integer (subseq path (length "/dev/input/event"))))
         (device (gethash id *device-table*)))
    (unless device
      (setf device (make-device-from-path path))
      (when (and device
                 (dev-gamepad-p (dev device))
                 (not (blacklisted-p device)))
        (setf (gethash id *device-table*) device)))
    device))

(defun init ()
  (cond (*device-notify*
         (list-devices))
        (T
         (cffi:use-foreign-library evdev)
         (let ((inotify (new-inotify :nonblock)))
           (assert (<= 0 inotify))
           (add-watch inotify "/dev/input" '(:create :delete))
           (setf *device-notify* inotify))
         (refresh-devices))))

(defun shutdown ()
  (when *device-notify*
    (u-close *device-notify*)
    (setf *device-notify* NIL)
    (mapc #'close-device (list-devices))
    T))

(defun list-devices ()
  (loop for device being the hash-values of *device-table*
        collect device))

(defun call-with-devices (function)
  (loop for device being the hash-values of *device-table*
        do (funcall function device)))

(defun refresh-devices (&optional function)
  (let ((to-delete (list-devices))
        (previous (list-devices))
        (function (ensure-function function)))
    (loop for path in (directory "/dev/input/event*")
          for device = (ensure-device path)
          do (setf to-delete (delete device to-delete)))
    (dolist (device to-delete)
      (funcall function :remove device)
      (close-device device))
    (dolist (device (set-difference (list-devices) previous))
      (funcall function :add device))
    (list-devices)))

(defun prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun call-with-polling (function fd &key timeout)
  (let ((tsec (etypecase timeout
                ((eql T) 0.5)
                ((eql NIL) 0)
                ((float 0) timeout))))
    (cffi:with-foreign-objects ((pollfd '(:struct pollfd)))
      (setf (pollfd-fd pollfd) fd)
      (setf (pollfd-events pollfd) :in)
      (setf (pollfd-revents pollfd) 0)
      ;; Even if timeout is T we do not want to block indefinitely,
      ;; or we would lose the ability to interrupt the thread at all.
      (loop for poll = (poll pollfd 1 (floor (* 1000 tsec)))
            do (check-errno (<= 0 poll))
               (when (< 0 poll)
                 (funcall function))
               (when (not (eql T timeout))
                 (return))
               ;; Force interrupt handling
               (finish-output)))))

(defun process-connect-events (function)
  (cffi:with-foreign-objects ((buffer '(:struct inotify) 32))
    (loop for read = (u-read *device-notify* buffer (* 1024 (cffi:foreign-type-size '(:struct inotify))))
          while (< 0 read)
          do (loop with i = 0
                   while (< i read)
                   do (let* ((struct (cffi:inc-pointer buffer i))
                             (path (cffi:foreign-string-to-lisp (cffi:foreign-slot-pointer struct '(:struct inotify) 'name)
                                                                :max-chars (inotify-length struct))))
                        (incf i (+ (inotify-length struct) (cffi:foreign-type-size '(:struct inotify))))
                        (when (prefix-p "event" path)
                          (cond ((find :create (inotify-mask struct))
                                 (let ((device (ensure-device (format NIL "/dev/input/~a" path))))
                                   (when device (funcall function :add device))))
                                ((find :delete (inotify-mask struct))
                                 (let* ((id (parse-integer (subseq path (length "event"))))
                                        (device (gethash id *device-table*)))
                                   (when device
                                     (unwind-protect
                                          (funcall function :remove device)
                                       (close-device device))))))))))))

(defun poll-devices (&key timeout function)
  (let ((function (ensure-function function)))
    (call-with-polling (lambda () (process-connect-events function)) *device-notify* :timeout timeout)))

(defun translate-event (function event device)
  (let ((time (logand (+ (* 1000 (event-sec event))
                         (floor (event-usec event) 1000))
                      (1- (ash 1 64)))))
    (case (event-type event)
      (:key
       (let* ((code (event-code event))
              (label (gethash code (button-map device))))
         (case (event-value event)
           (0 (signal-button-up function device time code label))
           (1 (signal-button-down function device time code label)))))
      (:absolute-axis
       (let* ((code (event-code event))
              (label (gethash code (axis-map device)))
              (value (event-value event))
              (info (get-axis-info (dev device) code))
              (float-value (axis-to-float label value (axis-info-minimum info) (axis-info-maximum info))))
         (signal-axis-move function device time code label float-value))))))

(defun call-with-device-events (function device)
  (let ((dev (dev device)))
    (cffi:with-foreign-object (event '(:struct event))
      (loop while (/= 0 (has-event-pending dev))
            for result = (next-event dev :normal event)
            do (case result
                 (:success
                  (translate-event function event device))
                 (:sync
                  (translate-event function event device)
                  (loop for next = (next-event dev :sync event)
                        until (eq next :again)
                        do (translate-event function event device)))
                 (:again)
                 (T
                  (linux-error result :message "Failed to read events.")))))))

(defun poll-events (device function &key timeout)
  (with-device-failures (device)
    (call-with-polling (lambda () (call-with-device-events function device))
                       (fd device) :timeout timeout)))

(defun rumble (device strength &key pan)
  (declare (ignore pan))
  (if (effect device)
      (with-device-failures (device)
        (let ((effect (effect device))
              (strength (floor (* #x7FFF (clamp 0 strength 1)))))
          (ecase (effect-type effect)
            (:constant
             (setf (ff-constant-level (effect-data effect)) strength))
            (:rumble
             (setf (ff-rumble-strong-magnitude (effect-data effect)) strength)
             (setf (ff-rumble-weak-magnitude (effect-data effect)) strength))
            (:periodic
             (setf (ff-periodic-magnitude (effect-data effect)) strength)))
          (check-errno (<= 0 (ioctl (fd device) :send-effect effect))
            (19 (return-from rumble :no-device)))
          (cffi:with-foreign-object (event '(:struct event))
            (setf (event-type event) :force-feedback)
            (setf (event-code event) (effect-id effect))
            (setf (event-value event) 1)
            (check-errno (< 0 (u-write (fd device) event (cffi:foreign-type-size '(:struct event))))
              (19 (return-from rumble :no-device)))
            :ok)))
      :unsupported))

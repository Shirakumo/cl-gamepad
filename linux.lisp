#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.impl)

(defvar *device-table* (make-hash-table :test 'eql))
(defvar *device-notify* NIL)

(defun dev-gamepad-p (dev)
  ;; KLUDGE: Heuristics
  (and (has-event-type dev :key)
       ;; Check if it at least has A / B
       (has-event-code dev :key #x130)
       (has-event-code dev :key #x131)
       ;; Check if it at least has a digital or analog DPAD
       (or (and (has-event-code dev :key #x222)
                (has-event-code dev :key #x223)
                (has-event-code dev :key #x220)
                (has-event-code dev :key #x221))
           (and (has-event-type dev :absolute-axis)
                (has-event-code dev :absolute-axis #x10)
                (has-event-code dev :absolute-axis #x11)))))

(defclass device (gamepad:device)
  ((id :initarg :id :reader id)
   (fd :initarg :fd :reader fd)
   (dev :initarg :dev :reader dev)))

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
  (let ((map (make-hash-table :test 'eql :size (length +labels+))))
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
                 (setf (gethash id map) label))))
    map))

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
          (make-instance 'device :id (parse-integer (subseq path (length "/dev/input/event")))
                                 :fd fd
                                 :dev dev
                                 :name (get-name dev)
                                 :vendor (get-id-vendor dev)
                                 :product (get-id-product dev)
                                 :version (get-id-version dev)
                                 :driver-version (get-driver-version dev)
                                 :button-map (device-button-map dev)
                                 :axis-map (device-axis-map dev)))))))

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
      (when (and device (dev-gamepad-p (dev device)))
        (setf (gethash id *device-table*) device)))
    device))

(defun init ()
  (unless *device-notify*
    (cffi:use-foreign-library evdev)
    (let ((inotify (new-inotify :nonblock)))
      (assert (<= 0 inotify))
      (add-watch inotify "/dev/input" '(:create :delete))
      (setf *device-notify* inotify))
    (refresh-devices)))

(defun shutdown ()
  (when *device-notify*
    (u-close *device-notify*)
    (setf *device-notify* NIL)
    (mapc #'close-device (list-devices))))

(defun list-devices ()
  (loop for device being the hash-values of *device-table*
        collect device))

(defun refresh-devices ()
  (let ((to-delete (list-devices)))
    (loop for path in (directory "/dev/input/event*")
          for device = (ensure-device path)
          do (setf to-delete (delete device to-delete)))
    (mapc #'close-device to-delete)
    (list-devices)))

(defun prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun call-with-polling (function fd &key timeout)
  (let ((tsec (etypecase timeout
                ((eql T) 1)
                ((eql NIL) 0)
                ((integer 0) timeout))))
    (cffi:with-foreign-objects ((pollfd '(:struct pollfd)))
      (setf (pollfd-fd pollfd) fd)
      (setf (pollfd-events pollfd) :in)
      (setf (pollfd-revents pollfd) 0)
      ;; Even if timeout is T we do not want to block indefinitely,
      ;; or we would lose the ability to interrupt the thread at all.
      (loop for poll = (poll pollfd 1 (floor (* 1000 tsec)))
            do (cond ((< 0 poll)
                      (funcall function))
                     ((< poll 0)
                      (error "Poll failed"))
                     ((not (eql T timeout))
                      (return)))))))

(defun process-connect-events ()
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
                                 (ensure-device (format NIL "/dev/input/~a" path)))
                                ((find :delete (inotify-mask struct))
                                 (let* ((id (parse-integer (subseq path (length "event"))))
                                        (device (gethash id *device-table*)))
                                   (when device (close-device device)))))))))))

(defun poll-devices (&key timeout)
  (call-with-polling #'process-connect-events *device-notify* :timeout timeout))

(defun translate-event (function event device)
  ;; TODO: Could probably make axis normalisation much faster based on AOT compilation.
  (let ((time (logand (+ (* 1000 (event-sec event))
                         (floor (event-usec event) 1000))
                      (1- (ash 1 64)))))
    (case (event-type event)
      (#.(cffi:foreign-enum-value 'event-type :key)
       (let* ((code (event-code event))
              (label (gethash code (button-map device))))
         (case (event-value event)
           (0 (signal-button-up function device time code label))
           (1 (signal-button-down function device time code label)))))
      (#.(cffi:foreign-enum-value 'event-type :absolute-axis)
       (let* ((code (event-code event))
              (label (gethash code (axis-map device)))
              (value (event-value event))
              (info (get-axis-info (dev device) code))
              (min (axis-info-minimum info))
              (max (axis-info-maximum info))
              (range (- max min))
              (float-value (case label
                             ((:l2 :r2) (float (/ (- value min) range)))
                             ((:l-v :r-v) (- 1f0 (* 2f0 (/ (- value min) range))))
                             (T (- (* 2f0 (/ (- value min) range)) 1f0)))))
         (signal-axis-move function device time code label float-value)))
      (T :other))))

(defun call-with-device-events (function device)
  (let ((dev (dev device)))
    (cffi:with-foreign-object (event '(:struct event))
      (case (next-event dev :normal event)
        (:success
         (translate-event function event device))
        (:sync
         (translate-event function event device)
         (loop for next = (next-event dev :sync event)
               until (eq next :again)
               do (translate-event function event device)))
        (:again)
        (T (error "Poll failed"))))))

(defun poll-events (device function &key timeout)
  (call-with-polling (lambda () (call-with-device-events function device)) (fd device) :timeout timeout))

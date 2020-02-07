#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.impl)

(defvar *device-table* (make-hash-table :test 'eql))
(defvar *directinput*)

(cffi:defcallback enum-devices enumerate-flag ((device :pointer) (user :pointer))
  (print (cffi:mem-ref device '(:struct device-instance)))
  (let* ((idx (enum-user-data-device-count user))
         (source (cffi:foreign-slot-pointer device '(:struct device-instance) 'guid))
         (target (cffi:mem-aptr (enum-user-data-device-array user) '(:struct guid) idx)))
    ;; GUID is 128 bits, copy in two uint64 chunks.
    (setf (cffi:mem-aref target :uint64 0) (cffi:mem-aref source :uint64 0))
    (setf (cffi:mem-aref target :uint64 1) (cffi:mem-aref source :uint64 1))
    (setf (enum-user-data-device-count user) (1+ idx))
    (if (< idx 255)
        :continue
        :stop)))

(cffi:defcallback enum-objects enumerate-flag ((object :pointer) (device :pointer))
  (device-unacquire device)
  (cffi:with-foreign-object (range '(:struct property-range))
    (setf (property-range-size range) (cffi:foreign-type-size '(:struct property-range)))
    (setf (property-range-header-size range) (cffi:foreign-type-size '(:struct property-header)))
    (setf (property-range-how range) :by-id)
    (setf (property-range-type range) (device-object-instance-type object))
    ;; One byte of range
    (setf (property-range-min range) -128)
    (setf (property-range-max range) +127)
    (check-return
     (device-set-property device DIPROP-RANGE range)))
  (cffi:with-foreign-object (dword '(:struct property-dword))
    (setf (property-dword-size dword) (cffi:foreign-type-size '(:struct property-dword)))
    (setf (property-dword-header-size dword) (cffi:foreign-type-size '(:struct property-header)))
    (setf (property-dword-how dword) :by-id)
    (setf (property-dword-type dword) (device-object-instance-type object))
    ;; No dead zone, handled in user code
    (setf (property-dword-data dword) 0)
    (check-return
     (device-set-property device DIPROP-DEADZONE dword)))
  :continue)

(defun prepare-dinput-device (dev)
  (check-return
   (device-set-cooperative-level dev (get-module-handle (cffi:null-pointer)) '(:background :exclusive)))
  (check-return
   (device-set-data-format dev data-format-joystick))
  (check-return
   (device-enum-objects dev (cffi:callback enum-objects) dev :axis))
  (check-return
   (device-acquire dev)))

(defclass device (gamepad::device)
  ((dev :initarg :dev :reader dev)))

(defun close-device (device)
  (device-unacquire (dev device))
  (com-release (dev device))
  (slot-makunbound device 'dev))

(defun make-device-from-dev (dev)
  (prepare-dinput-device dev)
  (make-instance 'device
                 :dev dev
                 :name TODO
                 :vendor TODO
                 :product TODO
                 :version TODO
                 :driver-version TODO))

(defun ensure-device (guid)
  (or (gethash (guid-integer guid) *device-table*)
      (cffi:with-foreign-object (dev :pointer)
        (check-return
         (directinput-create-device *directinput* guid dev (cffi:null-pointer)))
        (setf (gethash (guid-integer guid) *device-table*)
              (make-device-from-dev (cffi:mem-ref dev :pointer))))))

(defun list-devices ()
  (loop for device being the hash-values of *device-table*
        collect device))

(defun refresh-devices ()
  (let ((to-delete (list-devices)))
    (cffi:with-foreign-objects ((devices '(:struct guid) 256)
                                (enum-data '(:struct enum-user-data)))
      (setf (enum-user-data-directinput enum-data) *directinput*)
      (setf (enum-user-data-device-array enum-data) devices)
      (setf (enum-user-data-device-count enum-data) 0)
      (check-return
       (directinput-enum-devices *directinput* :game-controller (cffi:callback enum-devices) enum-data :attached-only))
      (loop for i from 0 below (enum-user-data-device-count enum-data)
            for device = (ensure-device (cffi:mem-aptr devices '(:struct guid) i))
            do (setf to-delete (delete device to-delete)))
      (mapc #'close-device to-delete)
      (list-devices))))

(defun init ()
  (unless (boundp '*directinput*)
    (cffi:use-foreign-library ole32)
    (cffi:use-foreign-library xinput)
    (cffi:use-foreign-library dinput)
    (check-return
     (co-initialize (cffi:null-pointer) :multi-threaded))
    (cffi:with-foreign-object (directinput :pointer)
      (check-return
       (create-direct-input (get-module-handle (cffi:null-pointer)) DINPUT-VERSION IID-IDIRECTINPUT8
                            directinput (cffi:null-pointer)))
      (setf *directinput* (cffi:mem-ref directinput :pointer))
      (refresh-devices))))

(defun shutdown ()
  (when (boundp '*directinput*)
    (mapc #'close-device (list-devices))
    (com-release *directinput*)
    (co-uninitialize)))

(defun poll-devices (&key timeout))
(defun poll-events (device function &key timeout))

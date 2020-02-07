#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.impl)

(defvar *directinput*)

(cffi:defcallback enum-devices enumerate-flag ((device :pointer) (user :pointer))
  (print (cffi:mem-ref device '(:struct device-instance)))
  (let* ((idx (enum-user-data-device-count user))
         (instance (cffi:mem-aptr (enum-user-data-device-array user) :pointer idx))
         (directinput (enum-user-data-directinput user))
         (guid (cffi:foreign-slot-pointer device '(:struct device-instance) 'guid)))
    (check-return
     (directinput-create-device directinput guid instance (cffi:null-pointer)))
    (setf (enum-user-data-device-count user) (1+ idx))
    (if (<= idx 255)
        :continue
        :stop)))

(defun enum-devices ()
  (cffi:with-foreign-objects ((devices ':pointer 256)
                              (enum-data '(:struct enum-user-data)))
    (setf (enum-user-data-directinput enum-data) *directinput*)
    (setf (enum-user-data-device-array enum-data) devices)
    (setf (enum-user-data-device-count enum-data) 0)
    (check-return
     (directinput-enum-devices *directinput* :game-controller (cffi:callback enum-devices) enum-data :attached-only))
    (loop for i from 0 below (enum-user-data-device-count enum-data)
          collect (cffi:mem-aref devices :pointer i))))

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
      (setf *directinput* (cffi:mem-ref directinput :pointer)))))

(defun shutdown ()
  (when (boundp '*directinput*)
    (com-release *directinput*)
    (co-uninitialize)))

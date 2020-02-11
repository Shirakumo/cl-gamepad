#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.impl)

(defclass device (gamepad:device)
  ())

(defun init ()
  (cffi:use-foreign-library 'corefoundation)
  (cffi:use-foreign-library 'iokit))

(defun shutdown ())

(defun list-devices ())

(defun poll-devices (&key timeout))

(defun poll-events (device function &key timeout))

;; (defun rumble (device &key)) ; TBD

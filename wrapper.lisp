#|
 This file is a part of cl-gamepad
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad)

(defun device-attached (device))
(defun device-removed (device))
(defun button-pressed (button time device))
(defun button-released (button time device))
(defun axis-moved (axis last-value value time device))

(defun init ()
  (gamepad-device-attach-func (callback device-attach-func) (null-pointer))
  (gamepad-device-remove-func (callback device-remove-func) (null-pointer))
  (gamepad-button-down-func (callback button-down-func) (null-pointer))
  (gamepad-button-up-func (callback button-up-func) (null-pointer))
  (gamepad-axis-move-func (callback axis-move-func) (null-pointer))
  (gamepad-init))

(defun shutdown ()
  (gamepad-shutdown))

(defun device-count ()
  (gamepad-num-devices))

(defun device (index)
  (gamepad-device-at index))

(defun detect-devices ()
  (gamepad-detect-devices))

(defun process-events ()
  (gamepad-process-events))

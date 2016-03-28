#|
 This file is a part of cl-gamepad
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad)

(defmacro define-alias (name original)
  `(setf (fdefinition ',name) (fdefinition ',original)))

(define-alias id device-id)
(define-alias vendor device-vendor)
(define-alias product device-product)
(define-alias description device-description)
(define-alias axis-count device-axis-count)
(define-alias button-count device-button-count)

(defun device-axis (device axis)
  (mem-aref (device-axis-states device) :float axis))

(defun device-axes (device)
  (let* ((size (device-axis-count device))
         (p (device-axis-states device))
         (array (make-array size)))
    (dotimes (i size array)
      (setf (aref array i) (mem-aref p :float i)))))

(defun device-button (device button)
  (mem-aref (device-button-states device) :bool button))

(defun device-buttons (device)
  (let* ((size (device-button-count device))
         (p (device-button-states device))
         (array (make-array size)))
    (dotimes (i size array)
      (setf (aref array i) (mem-aref p :float i)))))

(defun device-plist (device)
  `(:id ,(device-id device)
    :description ,(device-description device)
    :vendor ,(device-vendor device)
    :product ,(device-product device)
    :axis-states ,(device-axes device)
    :button-states ,(device-buttons device)))

(defun device-attached (device) (declare (ignore device)))
(defun device-removed (device) (declare (ignore device)))
(defun button-pressed (button time device) (declare (ignore button time device)))
(defun button-released (button time device) (declare (ignore button time device)))
(defun axis-moved (axis last-value value time device) (declare (ignore axis last-value value time device)))

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
  (gamepad-device-at-index index))

(defun devices ()
  (loop for i from 0 below (device-count)
        collect (device i)))

(defun detect-devices ()
  (gamepad-detect-devices))

(defun process-events ()
  (gamepad-process-events))

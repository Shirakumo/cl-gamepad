#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.impl)

;;; Weak enums do not signal errors when translating from unknown integers,
;;; instead just returning the integer. This is useful if we are only interested
;;; in a subset of enum values, or need to accept user/vendor defined codes that
;;; we cannot know about in advance.

(defclass weak-foreign-enum (cffi::foreign-enum)
  ())

(defmethod cffi:translate-from-foreign (value (type weak-foreign-enum))
  (or (cffi::%foreign-enum-keyword type value :errorp NIL)
      value))

(defun make-weak-foreign-enum (type-name base-type values)
  (multiple-value-bind (base-type keyword-values value-keywords)
      (cffi::parse-foreign-enum-like type-name base-type values)
    (make-instance 'weak-foreign-enum
                   :name type-name
                   :actual-type (cffi::parse-type base-type)
                   :keyword-values keyword-values
                   :value-keywords value-keywords)))

(defmacro defcenum* (name-and-options &body enum-list)
  (cffi::%defcenum-like name-and-options enum-list 'make-weak-foreign-enum))

;;; Allow relaying events to the user without allocating fresh event instances
(gamepad::define-global +button-down-event+ (gamepad::make-button-down NIL 0 0 NIL))
(gamepad::define-global +button-up-event+ (gamepad::make-button-up NIL 0 0 NIL))
(gamepad::define-global +axis-move-event+ (gamepad::make-axis-move NIL 0 0 NIL 0f0))

(defmacro %with-updated-event ((event) &body body)
  `(let ((event ,event))
     (setf (event-device event) device)
     (setf (event-time event) time)
     (setf (event-code event) code)
     (setf (event-label event) label)
     ,@body
     (funcall function event)))

(defun signal-button-down (function device time code label)
  (%with-updated-event (+button-down-event+)))

(defun signal-button-up (function device time code label)
  (%with-updated-event (+button-up-event+)))

(defun signal-axis-move (function device time code label value)
  (%with-updated-event (+axis-move-event+)
    (setf (event-value event) value)))

(defun id-label (id)
  (svref (load-time-value +labels+) id))

(define-compiler-macro id-label (&whole whole id &environment env)
  (if (constantp id env)
      `(load-time-value (svref (load-time-value +labels+) id))
      whole))

(defun label-id (label)
  (position label (load-time-value +labels+)))

(define-compiler-macro label-id (&whole whole label &environment env)
  (if (constantp label env)
      `(load-time-value (position ,label (load-time-value +labels+)))
      whole))

(defmacro with-device-failures ((device) &body body)
  `(restart-case
       (progn ,@body)
     (drop-device ()
       :report "Close and remove the device."
       (close-device ,device)
       NIL)))

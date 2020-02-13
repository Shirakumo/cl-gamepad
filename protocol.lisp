#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad)

(defmacro define-global (name value)
  #+sbcl `(sb-ext:defglobal ,name ,value)
  #-sbcl `(defvar ,name ,value))

(define-global +labels+ #(:a :b :c
                          :x :y :z
                          :l1 :l2 :l3
                          :r1 :r2 :r3
                          :dpad-l :dpad-r :dpad-u :dpad-d
                          :select :home :start
                          :l-h :l-v :r-h :r-v
                          :dpad-h :dpad-v
                          :tilt-x :tilt-y :tilt-z
                          :move-x :move-y :move-z
                          :wheel :gas :brake :throttle :rudder))

;;; Allow relaying events to the user without allocating fresh event instances
(define-global +button-down-event+ (make-button-down NIL 0 0 NIL))
(define-global +button-up-event+ (make-button-up NIL 0 0 NIL))
(define-global +axis-move-event+ (make-axis-move NIL 0 0 NIL 0f0))

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

(defmethod initialize-instance :after ((device device) &key vendor product version)
  ;; TODO: look up maps in database
  )

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

#-(or linux win32 darwin)
(progn
  (defun init ()
    (error "Unsupported platform."))
  
  (defun shutdown ()
    (error "Unsupported platform."))

  (defun list-devices ()
    ())

  (defun poll-devices (&key timeout)
    ())

  (defun poll-events (device function &key timeout)
    ())

  ;; TODO:
  ;; - Normalize dpad button events if controller only has axis and vice-versa
  (defun rumble (device strength &key pan))

  (defun (setf dead-zone) (min device axis))

  (defun (setf pressure-curve) (curve device axis)))

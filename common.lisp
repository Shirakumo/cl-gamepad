#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.impl)

;;; Allow relaying events to the user without allocating fresh event instances
(gamepad::define-global +button-down-event+ (gamepad::make-button-down NIL 0 0 NIL))
(gamepad::define-global +button-up-event+ (gamepad::make-button-up NIL 0 0 NIL))
(gamepad::define-global +axis-move-event+ (gamepad::make-axis-move NIL 0 0 NIL 0f0))

(defmacro %with-updated-event ((event) &body body)
  `(let ((event ,event))
     (setf (gamepad:event-device event) device)
     (setf (gamepad:event-time event) time)
     (setf (gamepad:event-code event) code)
     (setf (gamepad:event-label event) label)
     ,@body
     (funcall function event)))

(defun signal-button-down (function device time code label)
  (%with-updated-event (+button-down-event+)
    (when label
      (setf (sbit (button-states device) (label-id label)) 1))))

(defun signal-button-up (function device time code label)
  (%with-updated-event (+button-up-event+)
    (when label
      (setf (sbit (button-states device) (label-id label)) 0))))

(defun sibling-id (id)
  (ecase id
    (#.(label-id :l-h) (label-id :l-v))
    (#.(label-id :l-v) (label-id :l-h))
    (#.(label-id :r-h) (label-id :r-v))
    (#.(label-id :r-v) (label-id :r-h))))

(defun circular-rezone (value zone other-value)
  (declare (type single-float value zone other-value))
  (declare (optimize speed))
  (if (<= zone 0f0)
      value
      (let ((vector (+ (expt value 2) (expt other-value 2))))
        (if (< vector (expt zone 2))
            0f0
            (let ((len (sqrt vector)))
              (clamp -1f0 (* (/ value len) (/ (- len zone) (- 1 zone))) +1f0))))))

(defun square-rezone (value zone)
  (declare (type single-float value zone))
  (declare (optimize speed))
  (if (<= zone 0f0)
      value
      (if (< value zone)
          0f0
          (/ (- value zone) (- 1f0 zone)))))

(defun signal-axis-move (function device time code label value)
  (declare (optimize speed))
  (declare (type single-float value))
  (let* ((orientation (the single-float (gethash code (gamepad::orientation-map device) 1f0)))
         (value (* orientation value))
         (old-value value))
    ;; FIXME: ^ This is bad for obvious reasons.
    (%with-updated-event (+axis-move-event+)
      (when label
        (let ((id (label-id label))
              (zones (axis-dead-zones device))
              (raw-states (axis-raw-states device))
              (states (axis-states device))
              (ramps (axis-ramps device)))
          (declare (type (simple-array single-float) zones raw-states states)
                   (type (simple-array function) ramps))
          ;; Update raw state
          (setf (aref raw-states id) value)
          ;; Square rezone
          (setf value (square-rezone value (aref zones (+ 2 id))))
          ;; Circular rezone
          (case label
            ((:l-h :l-v)
             (setf value (circular-rezone value (aref zones 0) (aref raw-states (sibling-id id)))))
            ((:r-h :r-v)
             (setf value (circular-rezone value (aref zones 1) (aref raw-states (sibling-id id))))))
          ;; Apply ramp
          (setf value (the single-float (funcall (aref ramps id) value)))
          ;; Exit out if state did not change. This enforces the dead-zone
          (when (= value (aref states id))
            (return-from signal-axis-move))
          (setf old-value (aref states id))
          (setf (aref states id) value)))
      (setf (gamepad:event-old-value event) old-value)
      (setf (gamepad:event-value event) value))))

(defmacro with-device-failures ((device) &body body)
  `(restart-case
       (progn ,@body)
     (drop-device ()
       :report "Close and remove the device."
       (close-device ,device)
       NIL)))

(defun clamp (min value max)
  (cond ((< value min) min)
        ((< max value) max)
        (T value)))

(defun axis-to-float (label value min max)
  (let ((range (- max min)))
    (if (= range 0)
        0.0
        (case label
          ((:l2 :r2) (float (/ (- value min) range)))
          (T (- (* 2f0 (/ (- value min) range)) 1f0))))))

(defun ensure-function (function)
  (etypecase function
    (null (constantly NIL))
    (function function)
    (symbol (fdefinition function))))

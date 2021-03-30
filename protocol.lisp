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

(define-global +label-descriptions+
    '(:a "Lower button"
      :b "Right button"
      :x "Left button"
      :y "Upper button"
      :l1 "Upper left shoulder button"
      :l2 "Lower left shoulder button/trigger"
      :l3 "Left stick press in button"
      :r1 "Upper right shoulder button"
      :r2 "Lower right shoulder button/trigger"
      :r3 "Right stick press in button"
      :dpad-l "Left DPAD button"
      :dpad-r "Right DPAD button"
      :dpad-u "Up DPAD button"
      :dpad-d "Down DPAD button"
      :select "Left center button (select/share/minus)"
      :home "Middle center button (logo/home)"
      :start "Right center button (start/options/plus)"
      :l-h "Left stick horizontal axis"
      :l-v "Left stick vertical axis"
      :r-h "Right stick horizontal axis"
      :r-v "Right stick vertical axis"
      :dpad-h "DPAD horizontal axis"
      :dpad-v "DPAD vertical axis"
      :tilt-x "Tilt forward/backward"
      :tilt-y "Rotate flat"
      :tilt-z "Tilt left/right"
      :move-x "Move left/right"
      :move-y "Move up/down"
      :move-z "Move forward/backward"
      :wheel "Steering wheel rotation"
      :gas "Gas pedal"
      :brake "Brake pedal"
      :throttle "Throttle slider"
      :rudder "Rudder slider"))

(define-global +common-buttons+ #(:a :b :x :y :l1 :l2 :l3 :r1 :r2 :r3
                                  :dpad-l :dpad-r :dpad-u :dpad-d
                                  :select :home :start))

(define-global +common-axes+ #(:l2 :r2 :l-h :l-v :r-h :r-v :dpad-h :dpad-v))

(define-condition gamepad-error (error)
  ())

(defstruct event
  (device NIL)
  (time 0 :type (unsigned-byte 64))
  (code 0 :type (unsigned-byte 32))
  (label NIL :type symbol))

(defmethod print-object ((event event) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~a" (or (event-label event) (event-code event)))))

(defstruct (button-down (:include event)
                        (:constructor make-button-down (device time code label))))

(defstruct (button-up (:include event)
                      (:constructor make-button-up (device time code label))))

(defstruct (axis-move (:include event)
                      (:constructor make-axis-move (device time code label value))
                      (:conc-name event-))
  (value 0f0 :type single-float)
  (old-value 0f0 :type single-float))

(defmethod print-object ((event axis-move) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~a ~f" (or (event-label event) (event-code event)) (event-value event))))

(defclass device ()
  ((name :initarg :name :initform NIL :reader name)
   (vendor :initarg :vendor :initform NIL :reader vendor)
   (product :initarg :product :initform NIL :reader product)
   (version :initarg :version :initform NIL :reader version)
   (driver :initarg :driver :initform NIL :reader driver)
   (button-map :initarg :button-map :initform (make-hash-table :test 'eql) :accessor button-map)
   (axis-map :initarg :axis-map :initform (make-hash-table :test 'eql) :accessor axis-map)
   (orientation-map :initarg :orientation-map :initform (make-hash-table :test 'eql) :accessor orientation-map)
   (button-states :initform (make-array (length +labels+) :element-type 'bit :initial-element 0) :accessor button-states)
   (axis-states :initform (make-array (length +labels+) :element-type 'single-float :initial-element 0f0) :accessor axis-states)
   (axis-raw-states :initform (make-array (length +labels+) :element-type 'single-float :initial-element 0f0) :accessor axis-raw-states)
   (axis-ramps :initform (make-array (length +labels+) :initial-element #'identity) :accessor axis-ramps)
   (axis-dead-zones :initform (make-array (+ 2 (length +labels+)) :element-type 'single-float :initial-element 0f0) :accessor axis-dead-zones)))

(defmethod print-object ((device device) stream)
  (print-unreadable-object (device stream :type T)
    (format stream "~a" (name device))))

(defun id-label (id)
  (svref (load-time-value +labels+) id))

(define-compiler-macro id-label (&whole whole id &environment env)
  (if (constantp id env)
      `(load-time-value (svref (load-time-value +labels+) id))
      whole))

(defun label-id (label)
  (or (position label (load-time-value +labels+))
      (error "~s is not a valid label." label)))

(define-compiler-macro label-id (&whole whole label &environment env)
  (if (constantp label env)
      `(load-time-value (or (position ,label (load-time-value +labels+))
                            (error "~s is not a valid label." ,label)))
      whole))

(defun button (button device)
  (< 0 (bit (button-states device)) (label-id button)))

(define-compiler-macro button (&whole whole button device &environment env)
  (if (constantp button env)
      `(< 0 (bit (button-states ,device) (label-id ,button)))
      whole))

(defun axis (axis device)
  (aref (axis-states device) (label-id axis)))

(define-compiler-macro axis (&whole whole axis device &environment env)
  (if (constantp axis env)
      `(aref (axis-states ,device) (label-id ,axis))
      whole))

(defun dead-zone (device axis)
  (let ((id (case axis
              (:l 0)
              (:r 1)
              (T (+ 2 (label-id axis))))))
    (aref (axis-dead-zones device) id)))

(defun (setf dead-zone) (value device axis)
  (let ((id (case axis
              (:l 0)
              (:r 1)
              (T (+ 2 (label-id axis)))))
        (value (float value 0f0)))
    (check-type value (single-float 0f0 1f0))
    (setf (aref (axis-dead-zones device) id) value)))

(defun ramp (device axis)
  (aref (axis-ramps device) (label-id axis)))

(defun (setf ramp) (ramp device axis)
  (check-type ramp function)
  (setf (aref (axis-ramps device) (label-id axis)) ramp))

#-(or linux win32 darwin)
(progn
  (defun init ()
    (error "Unsupported platform."))
  
  (defun shutdown ()
    (error "Unsupported platform."))

  (defun call-with-devices (function)
    ())

  (defun list-devices ()
    ())

  (defun poll-devices (&key timeout)
    ())

  (defun poll-events (device function &key timeout)
    ())

  (defun rumble (device strength &key pan))

  ;; TODO:
  ;; - Normalize dpad button events if controller only has axis and vice-versa
  (defun (setf dead-zone) (min device axis))

  (defun (setf ramp) (curve device axis)))

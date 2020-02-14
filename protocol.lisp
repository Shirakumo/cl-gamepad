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
  (value 0f0 :type single-float))

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
   (axis-map :initarg :axis-map :initform (make-hash-table :test 'eql) :accessor axis-map)))

(defmethod print-object ((device device) stream)
  (print-unreadable-object (device stream :type T)
    (format stream "~a" (name device))))

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

  (defun rumble (device strength &key pan))

  ;; TODO:
  ;; - Normalize dpad button events if controller only has axis and vice-versa
  (defun (setf dead-zone) (min device axis))

  (defun (setf ramp) (curve device axis)))

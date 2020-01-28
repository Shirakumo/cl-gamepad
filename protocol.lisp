#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad)

(defvar *labels* #(:a :b :c
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

(defclass device ()
  ((name :initarg :name :initform NIL :reader name)
   (vendor :initarg :vendor :initform NIL :reader vendor)
   (product :initarg :product :initform NIL :reader product)
   (version :initarg :version :initform NIL :reader version)
   (driver-version :initarg :driver-version :initform NIL :reader driver-version)
   (button-map :initarg :button-map :initform (make-hash-table :test 'eql) :reader button-map)
   (axis-map :initarg :axis-map :initform (make-hash-table :test 'eql) :reader axis-map)))

(defmethod initialize-instance :after ((device device) &key vendor product version)
  ;; TODO: look up maps in database
  )

(defmethod print-object ((device device) stream)
  (print-unreadable-object (device stream :type T)
    (format stream "~a" (name device))))

(defun id-label (id)
  (svref (load-time-value *labels*) id))

(define-compiler-macro id-label (&whole whole id &environment env)
  (if (constantp id env)
      `(load-time-value (svref (load-time-value *labels*) id))
      whole))

(defun label-id (label)
  (position label (load-time-value *labels*)))

(define-compiler-macro label-id (&whole whole label &environment env)
  (if (constantp label env)
      `(load-time-value (position ,label (load-time-value *labels*)))
      whole))

;; process-devices
;; poll-devices
;; process-events
;; poll-events

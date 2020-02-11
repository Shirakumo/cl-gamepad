#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad)

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
   (button-map :initarg :button-map :initform (make-hash-table :test 'eql) :reader button-map)
   (axis-map :initarg :axis-map :initform (make-hash-table :test 'eql) :reader axis-map)))

(defmethod print-object ((device device) stream)
  (print-unreadable-object (device stream :type T)
    (format stream "~a" (name device))))

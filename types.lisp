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

(defstruct (button-down (:include event)
                        (:constructor make-button-down (device time code label))))

(defstruct (button-up (:include event)
                      (:constructor make-button-up (device time code label))))

(defstruct (axis-move (:include event)
                      (:constructor make-axis-move (device time code label value))
                      (:conc-name event-))
  (value 0f0 :type single-float))

(defclass device ()
  ((name :initarg :name :initform NIL :reader name)
   (vendor :initarg :vendor :initform NIL :reader vendor)
   (product :initarg :product :initform NIL :reader product)
   (version :initarg :version :initform NIL :reader version)
   (driver-version :initarg :driver-version :initform NIL :reader driver-version)
   (button-map :initarg :button-map :initform (make-hash-table :test 'eql) :reader button-map)
   (axis-map :initarg :axis-map :initform (make-hash-table :test 'eql) :reader axis-map)))

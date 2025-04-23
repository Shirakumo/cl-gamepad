(in-package #:org.shirakumo.fraf.gamepad.impl)

(defvar *default-device*)
(defvar *device-list* (make-array MAX-DEVICES :initial-element NIL))
(defvar *device-count* 0)

(defclass device (gamepad:device)
  ((id :initarg :id :accessor id)
   (index :initarg :index :accessor index)
   (pointer :initarg :pointer :accessor pointer)))

(defun init (&optional multiplayer)
  (unless (cffi:foreign-library-loaded-p 'nxgamepad)
    (cffi:use-foreign-library nxgamepad))
  (direct-init (if multiplayer 1 0))
  (refresh-devices))

(defun shutdown ()
  (when (cffi:foreign-library-loaded-p 'nxgamepad)
    (direct-shutdown))
  (setf *device-count* 0))

(defun call-with-devices (function)
  (loop for i from 0 below *device-count*
        do (funcall function (aref *device-list* i))))

(defun list-devices ()
  (loop for i from 0 below *device-count*
        collect (aref *device-list* i)))

(defun call-with-polling (function timeout)
  (typecase timeout
    (null
     (funcall function))
    ((eql T)
     (loop (funcall function)
           (sleep 0.0001)))
    (real
     (loop with start = (get-internal-real-time)
           with end = (* INTERNAL-TIME-UNITS-PER-SECOND timeout)
           for duration = (- (get-internal-real-time) start)
           for value = (funcall function)
           do (cond (value
                     (return value))
                    ((<= timeout duration)
                     (return)))))))

(defun make-label-table (labels)
  (let ((table (make-hash-table :test 'eql :size (length labels))))
    (dotimes (i (length labels) table)
      (setf (gethash (aref labels i) table) i))))

(defun make-device (index pointer)
  (make-instance 'device
                 :id (device-id pointer)
                 :index index
                 :pointer pointer
                 :name (device-name pointer)
                 :vendor 1406 ; Nintendo
                 :product (case (device-style pointer)
                            (:pro-controller 8201)
                            (:handheld 8192)
                            (:joy-dual 8206)
                            (:joy-left 8198)
                            (:joy-right 8199)
                            (T 8201))
                 :version 0
                 :driver :nx
                 :axis-map (make-label-table *nx-axis-labels*)
                 :button-map (make-label-table *nx-button-labels*)
                 :icon-type :nintendo-switch))

(setf *default-device*
      (make-instance 'device
                     :id 0
                     :index 0
                     :pointer (cffi:null-pointer)
                     :name "Nintendo Switch"
                     :vendor 1406
                     :product 8192
                     :version 0
                     :driver :nx
                     :axis-map (make-label-table *nx-axis-labels*)
                     :button-map (make-label-table *nx-button-labels*)
                     :icon-type :nintendo-switch))

(defun refresh-devices (&optional function)
  (cffi:with-foreign-object (list :pointer MAX-DEVICES)
    (let ((count (direct-list list MAX-DEVICES))
          (new-list (make-array MAX-DEVICES :initial-element NIL))
          (function (ensure-function function)))
      (declare (dynamic-extent new-list))
      (loop for i from 0 below count
            for pointer = (cffi:mem-aref list :pointer i)
            for id = (device-id pointer)
            for existing = (loop for i from 0 below *device-count*
                                 for device = (aref *device-list* i)
                                 do (when (eql id device) (return device)))
            do (cond (existing
                      (setf (index existing) i)
                      (setf (pointer existing) pointer)
                      (setf (aref new-list i) existing))
                     (T
                      (let ((dev (make-device i pointer)))
                        (setf (aref new-list i) dev)
                        (funcall function :add dev)))))
      (loop for i from 0 below *device-count*
            for dev = (aref *device-list* i)
            do (unless (find dev new-list)
                 (funcall function :remove dev)))
      (replace *device-list* new-list)
      (setf *device-count* count)
      (list-devices))))

(defun poll-devices (&key timeout function)
  (call-with-polling (lambda ()
                       (when (direct-refresh-devices)
                         (refresh-devices function)))
                     timeout))

(defun update-device (device &optional function)
  (let ((function (ensure-function function))
        (new-buttons (device-buttons (pointer device)))
        (new-axes (cffi:foreign-slot-pointer (pointer device) '(:struct device) 'analog))
        (existing-buttons (button-states device))
        (existing-axes (axis-states device))
        (button-labels *NX-BUTTON-LABELS*)
        (axis-labels *NX-AXIS-LABELS*))
    (dotimes (i BUTTON-COUNT)
      (let ((label (aref button-labels i)))
        (unless (eql (logbitp i new-buttons) (< 0 (sbit existing-buttons (label-id label))))
          (if (logbitp i new-buttons)
              (signal-button-down function device 0 i label)
              (signal-button-up function device 0 i label)))))
    (dotimes (i AXIS-COUNT)
      (let ((val (cffi:mem-aref new-axes :float i)))
        (unless (= val (aref existing-axes i))
          (signal-axis-move function device 0 i (aref axis-labels i) val))))))

(defun poll-events (device function &key timeout)
  (call-with-polling (lambda ()
                       (when (direct-poll-device (index device))
                         (update-device device function)))
                     timeout))

(defun rumble (device strength &key pan low)
  (declare (ignore pan))
  (if (< 0 (device-rumble (index device) (or low strength) strength))
      :ok
      :unsupported))

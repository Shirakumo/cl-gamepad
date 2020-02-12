#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.impl)

(defvar *hid-manager*)
(defvar *run-loop-mode*)
(defvar *device-table* (make-hash-table :test 'eql))

;;; Mini queue implementation, simply (TAIL-CONS . LIST)
(defun make-queue ()
  (let ((cons (cons NIL NIL)))
    (setf (car cons) cons)))

(defun queue-push (element queue)
  (let ((cons (cons element NIL)))
    (setf (cdr (car queue)) cons)
    (setf (car queue) cons)))

(defun queue-pop (queue)
  (pop (cdr queue)))

(cffi:defcallback device-match :void ((user :pointer) (result io-return) (sender :pointer) (dev :pointer))
  (declare (ignore user result sender))
  (ensure-device dev))

(cffi:defcallback device-remove :void ((user :pointer) (result io-return) (sender :pointer) (dev :pointer))
  (declare (ignore user result sender))
  (let ((device (gethash (cffi:pointer-address dev) *device-table*)))
    (when device
      (close-device device))))

(cffi:defcallback device-changed :void ((dev :pointer) (result io-return) (sender :pointer) (value :pointer))
  (declare (ignore result sender))
  (let ((device (gethash (cffi:pointer-address dev) *device-table*)))
    (when device
      (let* ((element (value-element value))
             (code (element-cookie element))
             (time (value-timestamp value))
             (int (value-int-value value))
             (queue (event-queue device)))
        (print (element-usage element))
        (case (element-type element)
          (:button
           (let ((label (gethash code (button-map device))))
             ;; (if (< 0 int)
             ;;     (queue-push (gamepad::make-button-down device time code label) queue)
             ;;     (queue-push (gamepad::make-button-up device time code label) queue))
             ))
          ((:axis :misc)
           (let ((label (gethash code (axis-map device)))
                 (min (element-logical-min element))
                 (max (element-logical-max element)))
             (case (element-usage element)
               (:hat-switch)
               (T
                )))))))))

(defclass device (gamepad:device)
  ((dev :initarg :dev :reader dev)
   (run-loop-mode :initarg :run-loop-mode :reader run-loop-mode)
   (event-queue :initform (make-queue) :reader event-queue)))

(defun create-device-from-dev (dev)
  (let* ((product-key (device-property dev (cfstr PRODUCT-KEY)))
         (name (if (or (cffi:null-pointer-p product-key)
                       (/= (string-type-id) (type-id product-key)))
                   "[Unknown]"
                   (cfstring->string product-key)))
         (mode (create-string name)))
    (register-value-callback dev (cffi:callback device-changed) dev)
    (device-schedule-with-run-loop dev (get-current-run-loop) mode)
    (make-instance 'device
                   :dev dev
                   :run-loop-mode mode
                   :name name
                   :vendor (device-int-property dev (cfstr VENDOR-ID-KEY))
                   :product (device-int-property dev (cfstr PRODUCT-ID-KEY))
                   :version (device-int-property dev (cfstr VERSION-NUMBER-KEY))
                   :driver :iokit)))

(defun ensure-device (dev)
  (or (gethash (cffi:pointer-address dev) *device-table*)
      (setf (gethash (cffi:pointer-address dev) *device-table*)
            (create-device-from-dev dev))))

(defun close-device (device)
  (device-unschedule-from-run-loop (dev device) (get-current-run-loop) (run-loop-mode device))
  (remhash (cffi:pointer-address (dev device)) *device-table*)
  (slot-makunbound device 'dev))

(defun init ()
  (cffi:use-foreign-library 'corefoundation)
  (cffi:use-foreign-library 'iokit)
  (unless (boundp '*run-loop-mode*)
    (setf *run-loop-mode* (create-string "device-run-loop")))
  (unless (boundp '*hid-manager*)
    (with-cf-objects ((k1 (cfstr DEVICE-USAGE-PAGE-KEY))
                      (k2 (cfstr DEVICE-USAGE-KEY))
                      (n1 (create-number :int32 (cffi:foreign-enum-value 'io-page :generic-desktop)))
                      (n2 (create-number :int32 (cffi:foreign-enum-value 'io-desktop-usage :joystick)))
                      (n3 (create-number :int32 (cffi:foreign-enum-value 'io-desktop-usage :gamepad)))
                      (n4 (create-number :int32 (cffi:foreign-enum-value 'io-desktop-usage :multi-axis-controller)))
                      (d1 (create-dictionary (list (cons k1 n1) (cons k2 n2))))
                      (d2 (create-dictionary (list (cons k1 n1) (cons k2 n3))))
                      (d3 (create-dictionary (list (cons k1 n1) (cons k2 n4))))
                      (a (create-array (list d1 d2 d3))))
      (let ((manager (create-hid-manager (cffi:null-pointer) 0)))
        (set-matching-multiple manager a)
        (register-device-match-callback manager (cffi:callback device-match) (cffi:null-pointer))
        (register-device-remove-callback manager (cffi:callback device-remove) (cffi:null-pointer))
        (open-manager manager 0)
        (manager-schedule-with-run-loop manager (get-current-run-loop) *run-loop-mode*)
        (run-loop *run-loop-mode* 0 T)))))

(defun shutdown ()
  (when (boundp '*hid-manager*)
    (let ((manager *hid-manager*))
      (makunbound *hid-manager*)
      (manager-unschedule-from-run-loop manager (get-current-run-loop) (cffi:null-pointer))
      (mapc #'close-device (list-devices))
      (close-manager manager 0)
      (release manager))))

(defun list-devices ()
  (loop for v being the hash-values of *device-table* collect v))

(defun call-with-polling (function mode timeout)
  (let ((s (etypecase timeout
             ((eql NIL) 0d0)
             ((eql T) 1d0)
             ((real 0) (float timeout 0d0)))))
    (loop (let ((result (run-loop mode s T)))
            (when (eql result :handled-source)
              (funcall function))
            (if (eql T timeout)
                (finish-output)
                (return))))))

(defmacro with-polling ((mode timeout) &body body)
  `(call-with-polling (lambda () ,@body) ,mode ,timeout))

(defun poll-devices (&key timeout)
  (with-polling (*run-loop-mode* timeout)))

(defun poll-events (device function &key timeout)
  (with-polling ((run-loop-mode device) timeout)
    (loop for event = (queue-pop (event-queue device))
          while event
          do (funcall function event))))

;; (defun rumble (device &key)) ; TBD

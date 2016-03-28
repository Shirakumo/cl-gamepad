#|
 This file is a part of cl-gamepad
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad)

(define-condition index-out-of-range (error)
  ((index :initarg :index :reader index)
   (range :initarg :range :reader range))
  (:report (lambda (c s) (format s "The index ~s is not within the range [~a ~a]."
                                 (index c) (car (range c)) (cdr (range c))))))

(defmacro define-alias (name original)
  `(progn
     (declaim (inline ,name))
     (defun ,name (device)
       (,original device))
     (define-compiler-macro ,name (device)
       (list ',original device))))

(define-alias id device-id)
(define-alias vendor device-vendor)
(define-alias product device-product)
(define-alias description device-description)
(define-alias axis-count device-axis-count)
(define-alias button-count device-button-count)

(defun axis (device axis)
  (check-type axis integer)
  (let ((count (1- (axis-count device))))
    (unless (<= 0 axis count)
      (error 'index-out-of-range :index axis :range (cons 0 count))))
  (cffi:mem-aref (device-axis-states device) :float axis))

(defun axes (device)
  (let* ((size (device-axis-count device))
         (p (device-axis-states device))
         (array (make-array size)))
    (dotimes (i size array)
      (setf (aref array i) (cffi:mem-aref p :float i)))))

(defun button (device button)
  (check-type button integer)
  (let ((count (1- (button-count device))))
    (unless (<= 0 button count)
      (error 'index-out-of-range :index button :range (cons 0 count))))
  (cffi:mem-aref (device-button-states device) :bool button))

(defun buttons (device)
  (let* ((size (device-button-count device))
         (p (device-button-states device))
         (array (make-array size)))
    (dotimes (i size array)
      (setf (aref array i) (cffi:mem-aref p :bool i)))))

(defun device-plist (device)
  `(:id ,(device-id device)
    :description ,(description device)
    :vendor ,(vendor device)
    :product ,(product device)
    :axis-states ,(axes device)
    :button-states ,(buttons device)))

(defun init ()
  (gamepad-device-attach-func (cffi:callback device-attach-func) (cffi:null-pointer))
  (gamepad-device-remove-func (cffi:callback device-remove-func) (cffi:null-pointer))
  (gamepad-button-down-func (cffi:callback button-down-func) (cffi:null-pointer))
  (gamepad-button-up-func (cffi:callback button-up-func) (cffi:null-pointer))
  (gamepad-axis-move-func (cffi:callback axis-move-func) (cffi:null-pointer))
  (gamepad-init))

(defun shutdown ()
  (gamepad-shutdown))

(defun device-count ()
  (gamepad-num-devices))

(defun device (index)
  (check-type index integer)
  (let ((count (1- (device-count))))
    (unless (<= 0 index count)
      (error 'index-out-of-range :index index :range (cons 0 count))))
  (gamepad-device-at-index index))

(defun devices ()
  (loop for i from 0 below (device-count)
        collect (device i)))

(defun detect-devices ()
  (gamepad-detect-devices))

(defun process-events ()
  (gamepad-process-events))

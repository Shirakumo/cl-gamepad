#|
 This file is a part of cl-gamepad
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad)

(defvar *init* NIL)

(define-condition index-out-of-range (error)
  ((index :initarg :index :reader index)
   (range :initarg :range :reader range))
  (:report (lambda (c s) (format s "The index ~s is not within the range [~a ~a]."
                                 (index c) (car (range c)) (cdr (range c))))))

;; We don't need any special handling for these, so let's
;; just wrap 'em up nice with as much inlining as possible.
(defmacro define-alias ((name original) args)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args
       (,original ,@args))
     (define-compiler-macro ,name ,args
       (list ',original ,@args))))

(define-alias (id device-id) (device))
(define-alias (vendor device-vendor) (device))
(define-alias (product device-product) (device))
(define-alias (description device-description) (device))
(define-alias (axis-count device-axis-count) (device))
(define-alias (button-count device-button-count) (device))
(define-alias (device-count gamepad-num-devices) ())
(define-alias (detect-devices gamepad-detect-devices) ())
(define-alias (process-events gamepad-process-events) ())

(defun axis (device axis)
  (check-type axis integer)
  (let ((count (1- (axis-count device))))
    (unless (<= 0 axis count)
      (error 'index-out-of-range :index axis :range (cons 0 count))))
  (* (cffi:mem-aref (device-axis-states device) :float axis)
     (cffi:mem-aref (device-map-axis-multipliers (device-device-map device)) :char axis)))

(defun axis-label (device axis)
  (if (< -1 axis DEVICE-MAP-MAX)
      (cffi:mem-aref (device-map-axes (device-device-map device)) 'axis axis)
      :unknown))

(defun axis-multiplier (device axis)
  (if (< -1 axis DEVICE-MAP-MAX)
      (cffi:mem-aref (device-map-axis-multipliers (device-device-map device)) :char axis)
      1))

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
  (< 0 (cffi:mem-aref (device-button-states device) :uint button)))

(defun button-label (device button)
  (if (< -1 button DEVICE-MAP-MAX)
      (cffi:mem-aref (device-map-buttons (device-device-map device)) 'button button)
      :unknown))

(defun buttons (device)
  (let* ((size (device-button-count device))
         (p (device-button-states device))
         (array (make-array size)))
    (dotimes (i size array)
      (setf (aref array i) (< 0 (cffi:mem-aref p :uint i))))))

(defun device-plist (device)
  `(:id ,(device-id device)
    :description ,(description device)
    :vendor ,(vendor device)
    :product ,(product device)
    :axis-states ,(axes device)
    :button-states ,(buttons device)))

(defun init ()
  (unless *init*
    (gamepad-device-attach-func (cffi:callback device-attach-func) (cffi:null-pointer))
    (gamepad-device-remove-func (cffi:callback device-remove-func) (cffi:null-pointer))
    (gamepad-button-down-func (cffi:callback button-down-func) (cffi:null-pointer))
    (gamepad-button-up-func (cffi:callback button-up-func) (cffi:null-pointer))
    (gamepad-axis-move-func (cffi:callback axis-move-func) (cffi:null-pointer))
    (gamepad-init)
    (setf *init* T)))

(defun shutdown ()
  (when *init*
    (gamepad-shutdown)
    (setf *init* NIL)))

(defun device (index)
  (check-type index integer)
  (let ((count (1- (device-count))))
    (unless (<= 0 index count)
      (error 'index-out-of-range :index index :range (cons 0 count))))
  (gamepad-device-at-index index))

(defun devices ()
  (loop for i from 0 below (device-count)
        collect (device i)))

(defun print-device (device stream)
  (flet ((actually-print (stream)
           (print-unreadable-object (device stream)
             (format stream "~s #~s (~s:~s) ~s"
                     'device (id device) (vendor device) (product device) (description device)))))
    (etypecase stream
      ((eql T) (actually-print *standard-output*))
      ((eql NIL) (with-output-to-string (stream)
                   (actually-print stream)))
      (stream (actually-print stream)))))

(defun device-map (vendor product)
  (let* ((map (gamepad-device-map vendor product))
         (buttons (device-map-buttons map))
         (axes (device-map-axes map))
         (mult (device-map-axis-multipliers map)))
    (list (list* :buttons
                 (loop for i from 0 below DEVICE-MAP-MAX
                       for button = (cffi:mem-aref buttons 'button i)
                       unless (eql button :unknown) collect (list i button)))
          (list* :axes
                 (loop for i from 0 below DEVICE-MAP-MAX
                       for axis = (cffi:mem-aref axes 'axis i)
                       for mul = (cffi:mem-aref mult :char i)
                       unless (eql axis :unknown) collect (list i axis mul))))))

(defun (setf device-map) (map vendor product)
  (cffi:with-foreign-object (dmap '(:struct device-map))
    (let ((buttons (device-map-buttons dmap))
          (axes (device-map-axes dmap))
          (mult (device-map-axis-multipliers dmap)))
      (loop for i from 0 below DEVICE-MAP-MAX
            for button = (second (assoc i (cdr (assoc :buttons map))))
            for (axis mul) = (cdr (assoc i (cdr (assoc :axes map))))
            do (setf (cffi:mem-aref buttons 'button i) (or button :unknown))
               (setf (cffi:mem-aref axes 'axis i) (or axis :unknown))
               (setf (cffi:mem-aref mult :char i) (or mul 1)))
      (gamepad-set-device-map vendor product dmap)
      map)))

(defun merge-maps (map defaults)
  (list (list* :buttons
               (loop for i from 0 below DEVICE-MAP-MAX
                     for button = (or (assoc i (cdr (assoc :buttons map)))
                                      (assoc i (cdr (assoc :buttons defaults))))
                     when button collect button))
        (list* :axes
               (loop for i from 0 below DEVICE-MAP-MAX
                     for axis = (or (assoc i (cdr (assoc :axes map)))
                                    (assoc i (cdr (assoc :axes defaults))))
                     when axis collect axis))))

(defun update-device-map (vendor product update)
  (setf (device-map vendor product) (merge-maps update (device-map vendor product))))

(defvar *vendor-product-names* (make-hash-table :test 'eql))

(defmacro define-gamepad (name (vendor product &key inherit) &body map)
  `(progn
     (setf (gethash ',name *vendor-product-names*) (list ,vendor ,product))
     (setf (device-map ,vendor ,product)
           ,(if inherit
                `(merge-maps ',map (apply #'device-map (gethash ',inherit *vendor-product-names*)))
                `',map))))

(defun device-name (device)
  (with-output-to-string (o)
    (loop for char across (cl-gamepad:description device)
          do (cond ((eql char #\ ) (write-char #\- o))
                   ((find char "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
                    (write-char (char-downcase char) o))))))

(defun gamepad-definition (device)
  (with-output-to-string (o)
    (format o "~&(define-gamepad ~a (~a ~a)"
            (device-name device) (cl-gamepad:vendor device) (cl-gamepad:product device))
    (loop for (thing . mappings) in (cl-gamepad:device-map (cl-gamepad:vendor device) (cl-gamepad:product device))
          do (format o "~&  (~(~s~)" thing)
             (loop for (id label mod) in mappings
                   do (format o "~&   (~2d ~(~s~)~@[ ~a~])" id label mod))
             (format o ")"))
    (format o ")")))

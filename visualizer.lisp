#|
 This file is a part of cl-gamepad
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:cl-gamepad-visualizer
  (:nicknames #:org.shirakumo.fraf.gamepad.visualizer)
  (:use #:cl+qt)
  (:export
   #:main))
(in-package #:org.shirakumo.fraf.gamepad.visualizer)
(in-readtable :qtools)

(defvar *main* NIL)

(defun text-size (painter text)
  (with-finalizing ((metrics (q+:make-qfontmetrics (q+:font painter))))
    (values (q+:width metrics text) (q+:height metrics))))

(defun draw-text-centered (painter text width height)
  (multiple-value-bind (w h) (text-size painter text)
    (q+:draw-text painter (round (- (/ width 2) (/ w 2))) (round (+ (/ height 2) (/ h 4))) text)))

(defmacro with-translation-saved ((painter) &body body)
  `(unwind-protect
        (progn (q+:save ,painter)
               ,@body)
     (q+:restore ,painter)))

(defun cl-gamepad:device-attached (device)
  (format NIL "~&> Device attached: ~a" (cl-gamepad:print-device device NIL))
  (when *main*
    (let ((gamepads (gamepads *main*)))
      (unless (find device gamepads :key #'device :test #'cffi:pointer-eq)
        (let ((gamepad (make-instance 'gamepad :device device)))
          (q+:insert-widget (slot-value *main* 'layout) (length gamepads) gamepad)
          (vector-push-extend gamepad gamepads))))))

(defun cl-gamepad:device-removed (device)
  (format NIL "~&> Device detached: ~a" (cl-gamepad:print-device device NIL))
  (when *main*
    (let* ((gamepads (gamepads *main*))
           (pos (position device gamepads :key #'device :test #'cffi:pointer-eq)))
      (q+:remove-item (slot-value *main* 'layout) (q+:item-at (slot-value *main* 'layout) pos))
      (finalize (aref gamepads pos))
      (loop for i from pos below (1- (length gamepads))
            do (setf (aref gamepads i) (aref gamepads (1+ i))))
      (decf (fill-pointer gamepads)))))

(define-widget main (QMainWindow)
  ((gamepads :initform (make-array 0 :adjustable T :fill-pointer T) :accessor gamepads)))

(define-initializer (main setup)
  (setf *main* main)
  (setf (q+:window-title main) "Gamepad Visualizer")
  (setf (q+:fixed-size main) (values 600 500)))

(define-finalizer (main teardown)
  (makunbound '*main*))

(define-subwidget (main timer) (q+:make-qtimer main)
  (setf (q+:single-shot timer) NIL)
  (q+:start timer 30))

(define-subwidget (main scroll) (q+:make-qscrollarea main)
  (setf (q+:widget-resizable scroll) T)
  (setf (q+:widget scroll) (q+:make-qwidget main))
  (setf (q+:central-widget main) scroll))

(define-subwidget (main layout) (q+:make-qvboxlayout (q+:widget scroll))
  (dotimes (i (cl-gamepad:device-count))
    (let ((device (cl-gamepad:device i)))
      (when device
        (cl-gamepad:device-attached device))))
  (q+:add-stretch layout))

(define-slot (main update) ()
  (declare (connected timer (timeout)))
  (cl-gamepad:detect-devices)
  (cl-gamepad:process-events)
  (q+:update (q+:widget scroll)))

(define-widget gamepad (QWidget)
  ((device :initarg :device :accessor device))
  (:default-initargs :device (error "DEVICE required.")))

(define-initializer (gamepad setup 100)
  (setf (q+:window-title gamepad) (format NIL "ID: ~a (~a:~a) ~a"
                                          (cl-gamepad:id device)
                                          (cl-gamepad:vendor device)
                                          (cl-gamepad:product device)
                                          (cl-gamepad:description device))))

(define-subwidget (gamepad description)
    (q+:make-qlabel (q+:window-title gamepad)))

(define-subwidget (gamepad axes-label)
    (q+:make-qlabel "Axes:"))

(define-subwidget (gamepad buttons-label)
    (q+:make-qlabel "Buttons:"))

(define-subwidget (gamepad export)
    (q+:make-qpushbutton "Export Device Map"))

(define-subwidget (gamepad layout) (q+:make-qvboxlayout gamepad)
  (setf (q+:margin layout) 0)
  (q+:add-widget layout description)
  (q+:add-widget layout axes-label)
  (let ((axes (q+:make-qgridlayout)))
    (setf (q+:margin axes) 0)
    (dotimes (i (cl-gamepad:axis-count device))
      (q+:add-widget axes (make-instance 'axis :device device :i i)
                     (floor i 5) (mod i 5)))
    (q+:add-layout layout axes))
  (q+:add-widget layout buttons-label)
  (let ((buttons (q+:make-qgridlayout)))
    (setf (q+:margin buttons) 0)
    (dotimes (i (cl-gamepad:button-count device))
      (q+:add-widget buttons (make-instance 'button :device device :i i)
                     (floor i 5) (mod i 5)))
    (q+:add-layout layout buttons))
  (q+:add-widget layout export))

(define-slot (gamepad export) ()
  (declare (connected export (clicked)))
  (q+:exec (make-instance 'text-display :text (cl-gamepad:gamepad-definition device))))

(define-widget text-display (QDialog)
  ())

(defmethod initialize-instance :after ((display text-display) &key text)
  (setf (q+:window-title display) "Text")
  (setf (q+:text (slot-value display 'text)) text))

(define-subwidget (text-display text) (q+:make-qtextedit text-display))

(define-subwidget (text-display ok) (q+:make-qpushbutton "&Ok"))

(define-subwidget (text-display layout) (q+:make-qvboxlayout text-display)
  (setf (q+:fixed-size text-display) (values 500 400))
  (q+:add-widget layout text)
  (q+:add-widget layout ok))

(define-subwidget (text-display font) (q+:make-qfont "Monospace" 10)
  (setf (q+:style-hint font) (q+:qfont.type-writer))
  (setf (q+:font text) font))

(define-slot (text-display ok) ()
  (declare (connected ok (clicked)))
  (q+:accept text-display))

(define-widget device-part (QPushButton)
  ((device :initarg :device :accessor device)
   (i :initarg :i :accessor i))
  (:default-initargs :device (error "DEVICE required.") :i (error "I required.")))

(define-initializer (device-part setup)
  (setf (q+:fixed-size device-part) (values 100 20)))

(define-widget chooser (QDialog)
  ((items :initarg :items :accessor items)
   (selected :initarg :selected :accessor selected))
  (:default-initargs :items () :selected NIL))

(define-initializer (chooser setup)
  (setf (q+:window-title chooser) "Label Chooser"))

(define-subwidget (chooser box) (q+:make-qcombobox chooser)
  (q+:add-items box (map 'list #'princ-to-string items))
  (let ((pos (position selected items)))
    (when pos (setf (q+:current-index box) pos))))

(define-subwidget (chooser ok) (q+:make-qpushbutton "&Ok"))
(define-subwidget (chooser cancel) (q+:make-qpushbutton "&Cancel"))

(define-subwidget (chooser layout) (q+:make-qgridlayout chooser)
  (q+:add-widget layout box 0 0 1 1)
  (q+:add-widget layout ok 0 1 1 1)
  (q+:add-widget layout cancel 1 1 1 1))

(define-slot (chooser ok) ()
  (declare (connected ok (clicked)))
  (setf (selected chooser) (elt (items chooser) (q+:current-index box)))
  (q+:accept chooser))

(define-slot (chooser cancel) ()
  (declare (connected cancel (clicked)))
  (q+:reject chooser))

(define-widget axis-chooser (QDialog chooser)
  ((multiplier :initarg :multiplier :accessor multiplier)))

(define-subwidget (axis-chooser mult) (q+:make-qcheckbox "Invert")
  (q+:add-widget (slot-value axis-chooser 'layout) mult 1 0 1 1)
  (if (= -1 multiplier) (setf (q+:checked mult) T)))

(defmethod %chooser-slot-ok :before ((axis-chooser axis-chooser))
  (setf (multiplier axis-chooser) (if (q+:is-checked (slot-value axis-chooser 'mult)) -1 1)))

(define-widget axis (QPushButton device-part)
  ())

(defun axis-label (device axis)
  (let ((label (cl-gamepad:axis-label device axis)))
    (princ-to-string (if (eql label :unknown) axis label))))

(define-override (axis paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter axis))
                    (black (q+:make-qcolor 0 0 0))
                    (green (q+:make-qcolor 100 255 100))
                    (white (q+:make-qcolor 255 255 255)))
    (q+:fill-rect painter 0 0 (q+:width axis) (q+:height axis) black)
    (q+:fill-rect painter (1- (round (+ (* (cl-gamepad:axis (device axis) (i axis)) 0.5 (q+:width axis)) (/ (q+:width axis) 2)))) 0 2 (q+:height axis) green)
    (setf (q+:color (q+:pen painter)) white)
    (draw-text-centered painter (axis-label (device axis) (i axis)) (q+:width axis) (q+:height axis))))

(define-slot (axis set-label) ()
  (declare (connected axis (clicked)))
  (with-finalizing ((chooser (make-instance 'axis-chooser :items (cffi:foreign-enum-keyword-list 'cl-gamepad-cffi:axis)
                                                          :selected (cl-gamepad:axis-label (device axis) (i axis))
                                                          :multiplier (cl-gamepad:axis-multiplier (device axis) (i axis)))))
    (when (= 1 (q+:exec chooser))
      (cl-gamepad:update-device-map (cl-gamepad:vendor (device axis)) (cl-gamepad:product (device axis))
                                    `((:axes (,(i axis) ,(selected chooser) ,(multiplier chooser))))))))

(define-widget button (QPushButton device-part)
  ())

(defun button-label (device button)
  (let ((label (cl-gamepad:button-label device button)))
    (princ-to-string (if (eql label :unknown) button label))))

(define-override (button paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter button))
                    (black (q+:make-qcolor 0 0 0))
                    (green (q+:make-qcolor 100 255 100))
                    (white (q+:make-qcolor 255 255 255)))
    (q+:fill-rect painter 0 0 (q+:width button) (q+:height button) black)
    (when (cl-gamepad:button (device button) (i button))
      (q+:fill-rect painter 2 2 (- (q+:width button) 4) (- (q+:height button) 4) green))
    (setf (q+:color (q+:pen painter)) white)
    (draw-text-centered painter (button-label (device button) (i button)) (q+:width button) (q+:height button))))

(define-slot (button set-label) ()
  (declare (connected button (clicked)))
  (with-finalizing ((chooser (make-instance 'chooser :items (cffi:foreign-enum-keyword-list 'cl-gamepad-cffi:button)
                                                     :selected (cl-gamepad:button-label (device button) (i button)))))
    (when (= 1 (q+:exec chooser))
      (cl-gamepad:update-device-map (cl-gamepad:vendor (device button)) (cl-gamepad:product (device button))
                                    `((:buttons (,(i button) ,(selected chooser))))))))

(defun main ()
  (cl-gamepad:init)
  (with-main-window (main 'main :main-thread NIL)))

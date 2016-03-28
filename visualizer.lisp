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

(defparameter *field-width* 50)
(defparameter *field-height* 20)
(defparameter *field-padding* 5)

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

(define-widget main (QWidget)
  ())

(define-initializer (main setup)
  (setf (q+:fixed-size main) (values 600 300)))

(define-subwidget (main timer) (q+:make-qtimer main)
  (setf (q+:single-shot timer) NIL)
  (q+:start timer (round (/ 1 30))))

(define-slot (main update) ()
  (declare (connected timer (timeout)))
  (cl-gamepad:detect-devices)
  (cl-gamepad:process-events)
  (q+:update main))

(define-override (main paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter main))
                    (white (q+:make-qcolor 255 255 255))
                    (black (q+:make-qcolor 0 0 0)))
    (setf (q+:color (q+:pen painter)) black)
    (q+:fill-rect painter (q+:rect main) white)
    (cond ((= 0 (cl-gamepad:device-count))
           (draw-text-centered painter "No gamepads detected at the moment..." (q+:width main) (q+:height main)))
          (T
           (q+:translate painter *field-padding* *field-padding*)
           (dotimes (i (cl-gamepad:device-count))
             (paint-device (cl-gamepad:device i) painter)
             (q+:translate painter 0 (* *field-height* 2)))))))

(defun paint-device (device painter)
  (q+:draw-text painter 0 20 (format NIL "ID: ~a (~a) ~a"
                                    (cl-gamepad:id device)
                                    (cl-gamepad:vendor device)
                                    (cl-gamepad:description device)))
  (q+:translate painter 20 20)
  (q+:draw-text painter 0 *field-height* (format NIL "Axis:"))
  (loop for i from 0 below (cl-gamepad:axis-count device)
        for col = (mod i 10)
        for x = (* col (+ *field-width* *field-padding*))
        do (when (= 0 col)
             (q+:translate painter 0 (+ *field-height* *field-padding*)))
           (with-translation-saved (painter)
             (q+:translate painter x 0)
             (paint-axis (princ-to-string i) (cl-gamepad:axis device i) painter)))
  (q+:translate painter 0 (+ *field-height* *field-padding*))
  (q+:draw-text painter 0 *field-height* (format NIL "Buttons:"))
  (loop for i from 0 below (cl-gamepad:button-count device)
        for col = (mod i 10)
        for x = (* col (+ *field-width* *field-padding*))
        do (when (= 0 col)
             (q+:translate painter 0 (+ *field-height* *field-padding*)))
           (with-translation-saved (painter)
             (q+:translate painter x 0)
             (paint-button (princ-to-string i) (cl-gamepad:button device i) painter))))

(defun paint-axis (name value painter)
  (with-finalizing ((black (q+:make-qcolor 0 0 0))
                    (green (q+:make-qcolor 100 255 100))
                    (white (q+:make-qcolor 255 255 255)))
    (q+:fill-rect painter 0 0 *field-width* *field-height* black)
    (q+:fill-rect painter (1- (round (+ (* value 0.5 *field-width*) (/ *field-width* 2)))) 0 2 *field-height* green)
    (setf (q+:color (q+:pen painter)) white)
    (draw-text-centered painter name *field-width* *field-height*)))

(defun paint-button (name value painter)
  (with-finalizing ((black (q+:make-qcolor 0 0 0))
                    (green (q+:make-qcolor 100 255 100))
                    (white (q+:make-qcolor 255 255 255)))
    (q+:fill-rect painter 0 0 *field-width* *field-height* black)
    (when value
      (q+:fill-rect painter 2 2 (- *field-width* 4) (- *field-height* 4) green))
    (setf (q+:color (q+:pen painter)) white)
    (draw-text-centered painter name *field-width* *field-height*)))

(defun main ()
  (cl-gamepad:init)
  (unwind-protect
       (with-main-window (main 'main :main-thread NIL))
    (cl-gamepad:shutdown)))

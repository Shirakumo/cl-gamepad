#|
 This file is a part of cl-gamepad
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad)

(define-gamepad xbox-360 (1118 654)
  (:buttons
   ( 0 :a)
   ( 1 :b)
   ( 2 :x)
   ( 3 :y)
   ( 4 :l1)
   ( 5 :r1)
   ( 6 :select)
   ( 7 :start)
   ( 8 :home)
   ( 9 :l)
   (10 :r))
  (:axes
   ( 0 :l-h)
   ( 1 :l-v)
   ( 2 :l2)
   ( 3 :r-h)
   ( 4 :r-v)
   ( 5 :r2)
   ( 6 :dpad-h)
   ( 7 :dpad-v)))

;; same as "xbox-360", all buttons where right in the visualizer
(define-gamepad microsoft-xbox-360-pad (1118 654)
  (:buttons
   ( 0 :a)
   ( 1 :b)
   ( 2 :x)
   ( 3 :y)
   ( 4 :l1)
   ( 5 :r1)
   ( 6 :select)
   ( 7 :start)
   ( 8 :home)
   ( 9 :l)
   (10 :r))
  (:axes
   ( 0 :l-h 1)
   ( 1 :l-v 1)
   ( 2 :l2 1)
   ( 3 :r-h 1)
   ( 4 :r-v 1)
   ( 5 :r2 1)
   ( 6 :dpad-h 1)
   ( 7 :dpad-v 1)))

(define-gamepad logitech-f310 (1133 49693 :inherit xbox-360)
  (:buttons)
  (:axes))

(define-gamepad steam-controller (10462 4604 :inherit xbox-360)
  (:buttons)
  (:axes))

(define-gamepad dualshock-3 (1356 616)
  (:buttons
   ( 0 :select)
   ( 1 :l)
   ( 2 :r)
   ( 3 :start)
   ( 4 :dpad-u)
   ( 5 :dpad-r)
   ( 6 :dpad-d)
   ( 7 :dpad-l)
   ( 8 :l2)
   ( 9 :r2)
   (10 :l1)
   (11 :r1)
   (12 :y) ; triangle
   (13 :b) ; circle
   (14 :a) ; cross
   (15 :x) ; square
   (16 :home))
  (:axes
   ( 0 :l-h)
   ( 1 :l-v)
   ( 2 :r-h)
   ( 3 :r-v)
   ( 8 :dpad-u)
   ( 9 :dpad-r)
   (10 :dpad-d)
   (11 :dpad-l)
   (12 :l2)
   (13 :r2)
   (14 :l1)
   (15 :r1)
   (16 :button-y) ; triangle
   (17 :button-b) ; circle
   (18 :button-a) ; cross
   (19 :button-x) ; square
   (23 :move-x)
   (24 :move-z)
   (25 :move-y)
   (26 :tilt-y)))

(define-gamepad dualshock-4 (1356 2508)
  (:buttons
   ( 0 :a)
   ( 1 :b)
   ( 2 :y)
   ( 3 :x)
   ( 4 :l1)
   ( 5 :r1)
   ( 6 :l2)
   ( 7 :r2)
   ( 8 :select)
   ( 9 :start)
   (10 :home)
   (11 :l)
   (12 :r))
  (:axes
   ( 0 :l-h)
   ( 1 :l-v)
   ( 2 :l2)
   ( 3 :r-h)
   ( 4 :r-v)
   ( 5 :r2)
   ( 6 :dpad-h)
   ( 7 :dpad-v)))

(define-gamepad buffalo-bsgp801 (1411 8288)
  (:buttons
   ( 0 :b)
   ( 1 :a)
   ( 2 :x)
   ( 3 :y)
   ( 4 :l1)
   ( 5 :r1)
   ( 6 :select)
   ( 7 :start))
  (:axes
   ( 0 :dpad-h)
   ( 1 :dpad-v)))

;; "iBUFFALO Classic USB Gamepad" (snes USB controller)
(define-gamepad usb2axis-8button-gamepad-- (1411 8288)
  (:buttons
   ( 0 :a)
   ( 1 :b)
   ( 2 :x)
   ( 3 :y)
   ( 4 :l1)
   ( 5 :r1)
   ( 6 :select)
   ( 7 :start))
  (:axes
   ( 0 :dpad-h 1)
   ( 1 :dpad-v 1)))

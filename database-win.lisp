#|
 This file is a part of cl-gamepad
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad)

(define-gamepad xbox-360 (1118 654)
  (:buttons
   ( 0 :dpad-u)
   ( 1 :dpad-d)
   ( 2 :dpad-l)
   ( 3 :dpad-r)
   ( 4 :start)
   ( 5 :select)
   ( 6 :l)
   ( 7 :r)
   ( 8 :l1)
   ( 9 :r1)
   (10 :a)
   (11 :b)
   (12 :x)
   (13 :y)
   (14 :home))
  (:axes
   ( 0 :l-h 1)
   ( 1 :l-v 1)
   ( 2 :r-h 1)
   ( 3 :r-v 1)
   ( 4 :l2 1)
   ( 5 :r2 1)))

(define-gamepad logitech-f310 (1133 49693 :inherit xbox-360)
  (:buttons)
  (:axes))

(define-gamepad steam-controller (10462 4604 :inherit xbox-360)
  (:buttons)
  (:axes))

(define-gamepad buffalo-bsgp301 (1411 8288)
  (:buttons
   ( 0 :b)
   ( 1 :a)
   ( 2 :y)
   ( 3 :x)
   ( 4 :l1)
   ( 5 :r1)
   ( 6 :select)
   ( 7 :start))
  (:axes
   ( 0 :dpad-v 1)
   ( 1 :dpad-h 1)))

(define-gamepad dualshock-3 (1356 616)
  (:buttons
   ( 0 :y)
   ( 1 :b)
   ( 2 :a)
   ( 3 :x)
   ( 4 :l2)
   ( 5 :r2)
   ( 6 :l1)
   ( 7 :r1)
   ( 8 :start)
   ( 9 :select)
   (10 :l)
   (11 :r)
   (12 :home))
  (:axes
   ( 0 :dpad-h 1)
   ( 1 :dpad-v 1)
   ( 2 :r2 -1)
   ( 3 :l2 -1)
   ( 4 :button-a -1)
   ( 5 :button-b -1)
   ( 6 :r-v 1)
   ( 7 :r-h 1)
   ( 8 :l-v 1)
   ( 9 :l-h 1)))

(define-gamepad dualshock-4 (1356 2508)
  (:buttons
   ( 0 :x)
   ( 1 :a)
   ( 2 :b)
   ( 3 :y)
   ( 4 :l1)
   ( 5 :r1)
   ( 6 :l2)
   ( 7 :r2)
   ( 8 :select)
   ( 9 :start)
   (10 :l1)
   (11 :r1)
   (12 :home))
  (:axes
   ( 0 :l-v 1)
   ( 1 :l-h 1)
   ( 2 :l-v 1)
   ( 3 :l-h 1)
   ( 4 :dpad-h 1)
   ( 5 :dpad-v 1)
   ( 6 :r2 1)
   ( 7 :l2 1)))

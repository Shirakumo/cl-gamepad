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

(define-gamepad logitech-f310 (1133 49693 :inherit xbox-360)
  (:buttons)
  (:axes))

(define-gamepad steam-controller (10462 4604 :inherit xbox-360)
  (:buttons)
  (:axes))

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

(define-gamepad pro-controller (1406 8201)
  (:buttons
   ( 0 :a)
   ( 1 :b)
   ( 2 :x)
   ( 3 :y)
   ( 4 :l1)
   ( 5 :r1)
   ( 6 :l2)
   ( 7 :r2)
   ( 8 :select)
   ( 9 :start)
   (10 :l)
   (11 :r)
   (12 :home)
   (13 :trigger))
  (:axes
   ( 0 :l-h 1)
   ( 1 :l-v 1)
   ( 2 :r-h 1)
   ( 3 :r-v 1)
   ( 4 :dpad-h 1)
   ( 5 :dpad-v 1)))

;; Assumed to be used as single-controller.
(define-gamepad joycon-l (1406 8198)
  (:buttons
   ( 0 :a)
   ( 1 :b)
   ( 2 :x)
   ( 3 :y)
   ( 4 :l1)
   ( 5 :r1)
   ( 8 :select)
   (10 :l)
   (13 :home)
   (14 :l1)
   (15 :l2))
  (:axes
   ( 4 :dpad-h 1)
   ( 5 :dpad-v 1)))

;; Assumed to be used as single-controller.
(define-gamepad joycon-r (1406 8199)
  (:buttons
   ( 0 :a)
   ( 1 :b)
   ( 2 :x)
   ( 3 :y)
   ( 4 :l1)
   ( 5 :r1)
   ( 9 :start)
   (11 :r)
   (12 :home)
   (14 :r1)
   (15 :r2))
  (:axes
   ( 4 :dpad-h 1)
   ( 5 :dpad-v 1)))

;; Using https://github.com/riking/joycon
(define-gamepad full-joycon (1406 8200)
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
   (12 :r)
   (13 :dpad-u)
   (14 :dpad-d)
   (15 :dpad-l)
   (16 :dpad-r))
  (:axes
   ( 0 :l-h 1)
   ( 1 :l-v 1)
   ( 2 :r-h 1)
   ( 3 :r-v 1)))

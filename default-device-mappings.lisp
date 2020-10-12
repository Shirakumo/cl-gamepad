;;;; Device mapping definitions
;;; This file is auto-generated. Do not edit it manually
;;; unless you know what you are doing. You can generate
;;; the file from current mappings in your Lisp image via
;;; 
;;;   (org.shirakumo.fraf.gamepad:save-device-mappings)
;;; 
;;; You can also interactively define new mappings using
;;; 
;;;   (org.shirakumo.fraf.gamepad:configure-device device)
;;;
(in-package "ORG.SHIRAKUMO.FRAF.GAMEPAD")

(define-device-mapping (:DINPUT 121 6)
  :name "JPD-UDV-01"
  :buttons (2 :A 1 :B 3 :X 0 :Y 4 :L1 6 :L2 10 :L3 5 :R1 7 :R2 11 :R3 8 :SELECT
            9 :START)
  :axes (0 :L-H 1 :L-V 2 :R-H 5 :R-V 8 :DPAD-V))

(define-device-mapping (:DINPUT 1133 49690)
  :name "Logitech G-UG15"
  :buttons (1 :A 2 :B 0 :X 3 :Y 4 :L1 6 :L2 5 :R1 7 :R2 8 :SELECT 9 :START)
  :axes (0 :DPAD-H 1 :DPAD-V))

(define-device-mapping (:DINPUT 1356 616)
  :name "Sony DualShock 3"
  :buttons (2 :A 1 :B 3 :X 0 :Y 6 :L1 4 :L2 10 :L3 7 :R1 5 :R2 11 :R3 9 :SELECT
            12 :HOME 8 :START)
  :axes (3 :L2 4 :R2 0 :L-H 1 :L-V 2 :R-H 5 :R-V 33 :DPAD-V 32 :DPAD-H))

(define-device-mapping (:DINPUT 1356 1476)
  :name "Sony DualShock 4"
  :buttons (1 :A 2 :B 0 :X 3 :Y 4 :L1 6 :L2 10 :L3 5 :R1 7 :R2 11 :R3 8 :SELECT
            12 :HOME 9 :START)
  :axes (3 :L2 4 :R2 0 :L-H 1 :L-V 2 :R-H 5 :R-V 8 :DPAD-V))

(define-device-mapping (:DINPUT 1356 2508)
  :name "Sony DualShock 4"
  :buttons (1 :A 2 :B 0 :X 3 :Y 4 :L1 6 :L2 10 :L3 5 :R1 7 :R2 11 :R3 8 :SELECT
              12 :HOME 9 :START)
  :axes (3 :L2 4 :R2 0 :L-H 1 :L-V 2 :R-H 5 :R-V 8 :DPAD-V))

(define-device-mapping (:DINPUT 1411 8288)
  :name "Buffalo BSGP801"
  :buttons (1 :A 0 :B 3 :X 2 :Y 4 :L1 5 :R1 6 :SELECT 7 :START)
  :axes (0 :DPAD-H 1 :DPAD-V))

(define-device-mapping (:EVDEV 1118 736)
  :name "8BitDo SN30 Pro+"
  :buttons (304 :A 305 :B 306 :X 307 :Y 308 :L1 312 :L3 309 :R1 313 :R3 310
            :SELECT 139 :HOME 311 :START)
  :axes (2 :L2 5 :L-H 0 :L-H 1 :R-H 3 :R-H 4 :R-V 16 :DPAD-H 17 :DPAD-V))

(define-device-mapping (:EVDEV 1133 49693)
  :name "Logitech Gamepad F310"
  :buttons (304 :A 305 :B 307 :X 308 :Y 310 :L1 317 :L3 311 :R1 318 :R3 314
            :SELECT 316 :HOME 315 :START)
  :axes (2 :L2 5 :R2 0 :L-H 1 :L-V 3 :R-H 4 :R-V 16 :DPAD-H 17 :DPAD-V))

(define-device-mapping (:EVDEV 1356 616)
  :name "Sony DualShock 3"
  :buttons (304 :A 305 :B 308 :X 307 :Y 310 :L1 312 :L2 317 :L3 311 :R1 313 :R2
            318 :R3 546 :DPAD-L 547 :DPAD-R 544 :DPAD-U 545 :DPAD-D 314 :SELECT
            316 :HOME 315 :START)
  :axes (2 :L2 5 :R2 0 :L-H 1 :L-V 3 :R-H 4 :R-V))

(define-device-mapping (:EVDEV 1356 1476)
  :name "Sony DualShock 4"
  :buttons (304 :A 305 :B 308 :X 307 :Y 310 :L1 312 :L2 317 :L3 311 :R1 313 :R2
            318 :R3 314 :SELECT 316 :HOME 315 :START)
  :axes (2 :L2 5 :R2 0 :L-H 1 :L-V 4 :R-H 3 :R-V 16 :DPAD-H 17 :DPAD-V))

(define-device-mapping (:EVDEV 1356 2508)
  :name "Sony DualShock 4"
  :buttons (304 :A 305 :B 308 :X 307 :Y 310 :L1 312 :L2 317 :L3 311 :R1 313 :R2
                318 :R3 314 :SELECT 316 :HOME 315 :START)
  :axes (2 :L2 5 :R2 0 :L-H 1 :L-V 3 :R-H 4 :R-V 16 :DPAD-H 17 :DPAD-V))

(define-device-mapping (:EVDEV 1406 8201)
  :name "Pro Controller"
  :buttons (304 :A 305 :B 306 :X 307 :Y 308 :L1 310 :L2 314 :L3 309 :R1 311 :R2
            315 :R3 312 :SELECT 316 :HOME 313 :START)
  :axes (16 :DPAD-H 17 :DPAD-V 0 :L-H 1 :L-V 3 :R-H 4 :R-V))

(define-device-mapping (:EVDEV 10462 4354)
  :name "Steam Controller"
  :buttons (304 :A 305 :B 307 :X 308 :Y 310 :L1 312 :L2 317 :L3 311 :R1 313 :R2
            318 :R3 290 :DPAD-L 289 :SELECT 314 :HOME 316 :HOME 315 :START)
  :axes (21 :L2 20 :R2 0 :L-H 1 :R-H 4 :R-V 3 :R-H))

(define-device-mapping (:EVDEV 10462 4418)
  :name "Wireless Steam Controller"
  :buttons (304 :A 305 :B 307 :X 308 :Y 310 :L1 312 :L2 317 :L3 311 :R1 313 :R2
            318 :R3 290 :DPAD-L 289 :SELECT 546 :DPAD-L 547 :DPAD-R 544 :DPAD-U
            545 :DPAD-D 314 :SELECT 316 :HOME 315 :START)
  :axes (21 :L2 20 :R2 0 :L-H 1 :L-V 3 :R-H 4 :R-V 16 :DPAD-H 17 :DPAD-V))

(define-device-mapping (:EVDEV 11720 24834)
  :name "8BitDo SN30 Pro+"
  :buttons (305 :A 304 :B 308 :X 307 :Y 310 :L1 312 :L2 317 :L3 311 :R1 313 :R2
            318 :R3 314 :SELECT 306 :HOME 315 :START)
  :axes (0 :L-H 1 :R-H 2 :R-H 5 :R-V 16 :DPAD-H 17 :DPAD-V))

(define-device-mapping (:IOKIT 1133 49674)
  :name "WingMan RumblePad"
  :buttons (589825 :A 589826 :B 589827 :C 589828 :X 589829 :Y 589830 :Z 589833
            :SELECT 589831 :L1 589832 :R1)
  :axes (65650 :DPAD-H 65651 :DPAD-V 65584 :L-H 65585 :L-V 65589 :R-H 65590
         :R-V 65586 :THROTTLE))

(define-device-mapping (:IOKIT 1356 616)
  :name "Sony DualShock 3"
  :buttons (589839 :A 589838 :B 589840 :X 589837 :Y 589835 :L1 589833 :L2
            589826 :L3 589836 :R1 589834 :R2 589827 :R3 589832 :DPAD-L 589830
            :DPAD-R 589829 :DPAD-U 589831 :DPAD-D 589825 :SELECT 589841 :HOME
            589828 :START)
  :axes (65584 :L-H 65585 :L-V 65586 :R-H 65589 :R-V))

(define-device-mapping (:IOKIT 1356 1476)
  :name "Sony DualShock 4"
  :buttons (589826 :A 589827 :B 589825 :X 589828 :Y 589829 :L1 589831 :L2
            589835 :L3 589830 :R1 589832 :R2 589836 :R3 589833 :SELECT 589837
            :HOME 589834 :START)
  :axes (65587 :L2 65588 :R2 65584 :L-H 65585 :L-V 65586 :R-H 65589 :R-V 65650
               :DPAD-H 65651 :DPAD-V))

(define-device-mapping (:IOKIT 1356 2508)
  :name "Sony DualShock 4"
  :buttons (589826 :A 589827 :B 589825 :X 589828 :Y 589829 :L1 589831 :L2
                   589835 :L3 589830 :R1 589832 :R2 589836 :R3 589833 :SELECT 589837
                   :HOME 589834 :START)
  :axes (65587 :L2 65588 :R2 65584 :L-H 65585 :L-V 65586 :R-H 65589 :R-V 65650
               :DPAD-H 65651 :DPAD-V))

(define-device-mapping (:IOKIT 1411 8288)
  :name "Buffalo BSGP801"
  :buttons (589826 :A 589825 :B 589828 :X 589827 :Y 589829 :L1 589830 :R1
                   589831 :SELECT 589832 :START)
  :axes (65584 :DPAD-H 65585 :DPAD-V))

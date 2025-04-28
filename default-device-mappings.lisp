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

(setf (blacklisted-p '(:EVDEV 1386 244)) "Wacom Cintiq 24HD Pad")
(setf (blacklisted-p '(:EVDEV 1386 817)) "Wacom Express Key Remote Pad")
(setf (blacklisted-p '(:EVDEV 9580 109)) "Huion Kamvas Pro 16")

(define-device-mapping (:DINPUT 121 6)
  :name "JPD-UDV-01"
  :icon-type :GENERIC-XBOX
  :buttons (2 :A 1 :B 3 :X 0 :Y 4 :L1 6 :L2 10 :L3 5 :R1 7 :R2 11 :R3 8 :SELECT
            9 :START)
  :axes (0 :L-H 1 :L-V 2 :R-H 5 :R-V 8 :DPAD-V)
  :orientations NIL)

(define-device-mapping (:DINPUT 1133 49690)
  :name "Logitech G-UG15"
  :icon-type :GENERIC-XBOX
  :buttons (1 :A 2 :B 0 :X 3 :Y 4 :L1 6 :L2 5 :R1 7 :R2 8 :SELECT 9 :START)
  :axes (0 :DPAD-H 1 :DPAD-V)
  :orientations NIL)

(define-device-mapping (:DINPUT 1356 616)
  :name "Sony DualShock 3"
  :icon-type :GENERIC-PLAYSTATION
  :buttons (2 :A 1 :B 3 :X 0 :Y 6 :L1 10 :L3 7 :R1 11 :R3 9 :SELECT 12 :HOME 8
            :START)
  :axes (3 :L2 4 :R2 0 :L-H 1 :L-V 2 :R-H 5 :R-V 32 :DPAD-H 33 :DPAD-V)
  :orientations NIL)

(define-device-mapping (:DINPUT 1356 1476)
  :name "Sony DualShock 4"
  :icon-type :DUALSHOCK-4
  :buttons (1 :A 2 :B 0 :X 3 :Y 4 :L1 10 :L3 5 :R1 11 :R3 8 :SELECT 12 :HOME 9
            :START)
  :axes (3 :L2 4 :R2 0 :L-H 1 :L-V 2 :R-H 5 :R-V 7 :DPAD-H 8 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 3 1.0 4 1.0 5 -1.0 7 1.0 8 1.0))

(define-device-mapping (:DINPUT 1356 2508)
  :name "Sony DualShock 4"
  :icon-type :DUALSHOCK-4
  :buttons (1 :A 2 :B 0 :X 3 :Y 4 :L1 10 :L3 5 :R1 11 :R3 8 :SELECT 12 :HOME 9
            :START)
  :axes (3 :L2 4 :R2 0 :L-H 1 :L-V 2 :R-H 5 :R-V 7 :DPAD-H 8 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 3 1.0 4 1.0 5 -1.0 7 1.0 8 1.0))

(define-device-mapping (:DINPUT 1356 3302)
  :name "Sony DualSense"
  :icon-type :DUALSHOCK-4
  :buttons (1 :A 2 :B 0 :X 3 :Y 4 :L1 10 :L3 5 :R1 11 :R3 8 :SELECT 12 :HOME 9
            :START)
  :axes (3 :L2 4 :R2 0 :L-H 1 :L-V 2 :R-H 5 :R-V 8 :DPAD-H 9 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 3 1.0 4 1.0 5 -1.0 8 1.0 9 1.0))

(define-device-mapping (:DINPUT 1356 3570)
  :name "Sony DualSense Edge"
  :icon-type :DUALSHOCK-4
  :buttons (1 :A 2 :B 0 :X 3 :Y 4 :L1 10 :L3 5 :R1 11 :R3 8 :SELECT 12 :HOME 9
              :START)
  :axes (3 :L2 4 :R2 0 :L-H 1 :L-V 2 :R-H 5 :R-V 8 :DPAD-H 9 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 3 1.0 4 1.0 5 -1.0 8 1.0 9 1.0))

(define-device-mapping (:DINPUT 1406 8201)
  :name "Pro Controller"
  :icon-type :NINTENDO-SWITCH
  :buttons (0 :A 1 :B 2 :X 3 :Y 4 :L1 6 :L2 10 :L3 5 :R1 7 :R2 11 :R3 8 :SELECT
            12 :HOME 9 :START)
  :axes (0 :L-H 1 :L-V 3 :R-H 4 :R-V 7 :DPAD-H 8 :DPAD-V)
  :orientations (0 1.0 1 -1.0 3 1.0 4 -1.0 8 1.0))

(define-device-mapping (:DINPUT 1411 8288)
  :name "Buffalo BSGP801"
  :icon-type :GENERIC-NINTENDO
  :buttons (1 :A 0 :B 3 :X 2 :Y 4 :L1 5 :R1 6 :SELECT 7 :START)
  :axes (0 :DPAD-H 1 :DPAD-V)
  :orientations (0 1.0 1 -1.0))

(define-device-mapping (:DINPUT 3853 146)
  :name "Hori Pokken Controller"
  :icon-type :GENERIC-NINTENDO
  :buttons (1 :A 2 :B 0 :X 3 :Y 4 :L1 6 :L2 10 :L3 5 :R1 7 :R2 11 :R3 8 :SELECT
            12 :HOME 9 :START 13 :CAPTURE)
  :axes (8 :DPAD-H 9 :DPAD-V)
  :orientations NIL)

(define-device-mapping (:DINPUT 4292 33472)
  :name "Multi-Platform Gamepad GP2040-CE (D-Input)"
  :icon-type :GENERIC-XBOX
  :buttons (1 :A 2 :B 0 :X 3 :Y 4 :L1 6 :L2 10 :L3 5 :R1 7 :R2 11 :R3 8 :SELECT
            12 :HOME 9 :START 13 :CAPTURE)
  :axes (8 :DPAD-H 9 :DPAD-V)
  :orientations NIL)

(define-device-mapping (:DINPUT 5426 1025)
  :name "Multi-Platform Gamepad GP2040-CE (PS4)"
  :icon-type :GENERIC-PLAYSTATION
  :buttons (1 :A 2 :B 0 :X 3 :Y 4 :L1 6 :L2 10 :L3 5 :R1 7 :R2 11 :R3 8 :SELECT
            12 :HOME 9 :START 13 :CAPTURE)
  :axes (3 :L2 4 :R2 8 :DPAD-H 9 :DPAD-V)
  :orientations NIL)

(define-device-mapping (:EVDEV 1118 654)
  :name "Microsoft X-Box 360 pad"
  :icon-type :GENERIC-XBOX
  :buttons (304 :A 305 :B 307 :X 308 :Y 310 :L1 317 :L3 311 :R1 318 :R3 314
            :SELECT 316 :HOME 315 :START)
  :axes (2 :L2 5 :R2 0 :L-H 1 :L-V 3 :R-H 4 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 3 1.0 4 -1.0 5 1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 1118 721)
  :name "Microsoft X-Box One pad"
  :icon-type :GENERIC-XBOX
  :buttons (304 :A 305 :B 307 :X 308 :Y 310 :L1 317 :L3 311 :R1 318 :R3 314
            :SELECT 316 :HOME 315 :START)
  :axes (2 :L2 5 :R2 0 :L-H 1 :L-V 3 :R-H 4 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 -1.0 1 -1.0 2 1.0 3 -1.0 4 -1.0 5 1.0 16 -1.0 17 1.0))

(define-device-mapping (:EVDEV 1118 736)
  :name "8BitDo SN30 Pro+"
  :icon-type :GENERIC-NINTENDO
  :buttons (304 :A 305 :B 306 :X 307 :Y 308 :L1 312 :L3 309 :R1 313 :R3 310
            :SELECT 139 :HOME 311 :START)
  :axes (2 :L2 5 :R2 0 :L-H 1 :L-V 3 :R-H 4 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 3 1.0 4 -1.0 5 1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 1118 2816)
  :name "Xbox Elite Series 2"
  :icon-type :XBOX-ONE
  :buttons (304 :A 305 :B 307 :X 308 :Y 310 :L1 317 :L3 311 :R1 318 :R3 314
            :SELECT 316 :HOME 315 :START)
  :axes (2 :L2 5 :R2 0 :L-H 1 :L-V 3 :R-H 4 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 3 1.0 4 -1.0 5 1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 1118 2821)
  :name "Xbox Elite Controller Wireless"
  :icon-type :XBOX-ONE
  :buttons (304 :A 305 :B 307 :X 308 :Y 310 :L1 317 :L3 311 :R1 318 :R3 158
            :SELECT 316 :HOME 315 :START)
  :axes (10 :L2 9 :R2 0 :L-H 1 :L-V 2 :R-H 5 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 5 -1.0 9 1.0 10 1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 1118 2834)
  :name "Microsoft X-Box One pad"
  :icon-type :XBOX-ONE
  :buttons (304 :A 305 :B 307 :X 308 :Y 310 :L1 317 :L3 311 :R1 318 :R3 314
            :SELECT 167 :HOME 315 :START)
  :axes (2 :L2 5 :R2 0 :L-H 1 :L-V 3 :R-H 4 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 3 1.0 4 -1.0 5 1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 1133 49693)
  :name "Logitech Gamepad F310"
  :icon-type :GENERIC-XBOX
  :buttons (304 :A 305 :B 307 :X 308 :Y 310 :L1 317 :L3 311 :R1 318 :R3 314
            :SELECT 316 :HOME 315 :START)
  :axes (2 :L2 5 :R2 0 :L-H 1 :L-V 3 :R-H 4 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 3 1.0 4 -1.0 5 1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 1356 616)
  :name "Sony DualShock 3"
  :icon-type :GENERIC-PLAYSTATION
  :buttons (304 :A 305 :B 308 :X 307 :Y 310 :L1 312 :L2 317 :L3 311 :R1 313 :R2
            318 :R3 546 :DPAD-L 547 :DPAD-R 544 :DPAD-U 545 :DPAD-D 314 :SELECT
            316 :HOME 315 :START)
  :axes (2 :L2 5 :R2 0 :L-H 1 :L-V 3 :R-H 4 :R-V)
  :orientations (0 1.0 1 -1.0 2 1.0 3 1.0 4 -1.0 5 1.0))

(define-device-mapping (:EVDEV 1356 1476)
  :name "Sony DualShock 4"
  :icon-type :DUALSHOCK-4
  :buttons (304 :A 305 :B 308 :X 307 :Y 310 :L1 312 :L2 317 :L3 311 :R1 313 :R2
            318 :R3 314 :SELECT 316 :HOME 315 :START)
  :axes (2 :L2 5 :R2 0 :L-H 1 :L-V 4 :R-H 3 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 3 1.0 4 -1.0 5 1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 1356 2508)
  :name "Sony DualShock 4"
  :icon-type :DUALSHOCK-4
  :buttons (304 :A 305 :B 308 :X 307 :Y 310 :L1 312 :L2 317 :L3 311 :R1 313 :R2
            318 :R3 314 :SELECT 316 :HOME 315 :START)
  :axes (2 :L2 5 :R2 0 :L-H 1 :L-V 3 :R-H 4 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 3 1.0 4 -1.0 5 1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 1356 3302)
  :name "Sony DualSense"
  :icon-type :DUALSHOCK-4
  :buttons (304 :A 305 :B 308 :X 307 :Y 310 :L1 317 :L3 311 :R1 318 :R3 314
            :SELECT 316 :HOME 315 :START)
  :axes (2 :L2 5 :R2 0 :L-H 1 :L-V 3 :R-H 4 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 3 1.0 4 -1.0 5 1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 1356 3570)
  :name "Sony DualSense Edge"
  :icon-type :DUALSHOCK-4
  :buttons (304 :A 305 :B 308 :X 307 :Y 310 :L1 317 :L3 311 :R1 318 :R3 314
                :SELECT 316 :HOME 315 :START)
  :axes (2 :L2 5 :R2 0 :L-H 1 :L-V 3 :R-H 4 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 3 1.0 4 -1.0 5 1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 1406 816)
  :name "Wii U Pro Controller"
  :icon-type :NINTENDO-SWITCH
  :buttons (305 :A 304 :B 307 :X 308 :Y 310 :L1 312 :L2 317 :L3 311 :R1 313 :R2
            318 :R3 546 :DPAD-L 547 :DPAD-R 544 :DPAD-U 545 :DPAD-D 314 :SELECT
            316 :HOME 315 :START)
  :axes (0 :L-H 1 :L-V 3 :R-H 4 :R-V)
  :orientations (0 1.0 1 -1.0 3 1.0 4 -1.0))

(define-device-mapping (:EVDEV 1406 8201)
  :name "Pro Controller"
  :icon-type :NINTENDO-SWITCH
  :buttons (304 :A 305 :B 308 :X 307 :Y 310 :L1 312 :L2 317 :L3 311 :R1 313 :R2
            318 :R3 314 :SELECT 316 :HOME 315 :START)
  :axes (0 :L-H 1 :L-V 3 :R-H 4 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 3 1.0 4 -1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 2064 3)
  :name "Sony DualShock 2"
  :icon-type :GENERIC-PLAYSTATION
  :buttons (290 :A 289 :B 291 :X 288 :Y 294 :L1 292 :L2 298 :L3 295 :R1 293 :R2
            299 :R3 296 :SELECT 297 :START)
  :axes (0 :L-H 1 :L-V 5 :R-H 2 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 -1.0 5 1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 2079 58369)
  :name "Generic SNES-style USB Gamepad"
  :icon-type :GENERIC-NINTENDO
  :buttons (290 :A 289 :B 291 :X 288 :Y 292 :L1 293 :R1 296 :SELECT 297 :START)
  :axes (0 :DPAD-H 1 :DPAD-V)
  :orientations (0 1.0 1 -1.0))

(define-device-mapping (:EVDEV 3853 95)
  :name "HORI CO.,LTD. Fighting Commander 4"
  :icon-type :DUALSHOCK-4
  :buttons (305 :A 306 :B 304 :X 307 :Y 308 :L1 310 :L2 314 :L3 309 :R1 311 :R2
            315 :R3 312 :SELECT 316 :HOME 313 :START)
  :axes (0 :L-H 1 :L-V 2 :R-H 5 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 5 -1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 8406 8203)
  :name "Generic X-Box pad"
  :icon-type :GENERIC-XBOX
  :buttons (304 :A 305 :B 307 :X 308 :Y 310 :L1 317 :L3 311 :R1 318 :R3 314
            :SELECT 315 :START)
  :axes (2 :L2 5 :R2 0 :L-H 1 :L-V 3 :R-H 4 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 3 1.0 4 -1.0 5 1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 8406 42769)
  :name "PowerA GameCube-style Switch Controller"
  :icon-type :GENERIC-NINTENDO
  :buttons (306 :A 307 :B 305 :X 304 :Y 308 :L1 310 :L2 314 :L3 309 :R1 311 :R2
            315 :R3 312 :SELECT 316 :HOME 313 :START)
  :axes (0 :L-H 1 :L-V 2 :R-H 5 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 5 -1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 9571 1397)
  :name "Retro Fighters Defender"
  :icon-type :GENERIC-PLAYSTATION
  :buttons (306 :A 305 :B 307 :X 304 :Y 308 :L1 310 :L2 314 :L3 309 :R1 311 :R2
            315 :R3 312 :SELECT 313 :START)
  :axes (0 :L-H 1 :L-V 2 :R-H 5 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 5 -1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 10462 4354)
  :name "Steam Controller"
  :icon-type :GENERIC-XBOX
  :buttons (304 :A 305 :B 307 :X 308 :Y 310 :L1 312 :L2 317 :L3 311 :R1 313 :R2
            318 :R3 290 :DPAD-L 289 :SELECT 314 :HOME 316 :HOME 315 :START)
  :axes (21 :L2 20 :R2 0 :L-H 1 :R-H 3 :R-H 4 :R-V)
  :orientations (0 1.0 1 -1.0 3 1.0 4 -1.0 20 -1.0 21 -1.0))

(define-device-mapping (:EVDEV 10462 4418)
  :name "Steam Controller Wireless"
  :icon-type :GENERIC-XBOX
  :buttons (304 :A 305 :B 307 :X 308 :Y 310 :L1 312 :L2 317 :L3 311 :R1 313 :R2
            318 :R3 290 :DPAD-L 289 :DPAD-D 314 :SELECT 316 :HOME 315 :START)
  :axes (21 :L2 20 :L-H 0 :L-H 1 :L-V 3 :R-H 4 :R-V)
  :orientations (0 1.0 1 -1.0 3 1.0 4 -1.0 20 -1.0 21 -1.0))

(define-device-mapping (:EVDEV 10462 4607)
  :name "Steam Deck"
  :icon-type :XBOX-ONE
  :buttons (304 :A 305 :B 307 :X 308 :Y 310 :L1 317 :L3 311 :R1 318 :R3 314
            :SELECT 315 :START)
  :axes (2 :L2 5 :R2 0 :L-H 1 :L-V 3 :R-H 4 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 3 1.0 4 -1.0 5 1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 11720 12555)
  :name "8BitDo Ultimate 2"
  :icon-type :GENERIC-XBOX
  :buttons (304 :A 305 :B 307 :X 308 :Y 310 :L1 317 :L3 311 :R1 318 :R3 314
            :SELECT 316 :HOME 315 :START)
  :axes (2 :L2 5 :R2 0 :L-H 1 :L-V 3 :R-H 4 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 3 1.0 4 -1.0 5 1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 11720 24594)
  :name "8BitDo Ultimate 2 Wireless"
  :icon-type :GENERIC-XBOX
  :buttons (304 :A 305 :B 307 :X 308 :Y 310 :L1 312 :L2 317 :L3 311 :R1 313 :R2
            318 :R3 314 :SELECT 316 :HOME 315 :START)
  :axes (10 :R2 9 :R2 0 :L-H 1 :L-V 2 :R-H 5 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 5 -1.0 9 1.0 10 1.0 16 1.0 17 -1.0))

(define-device-mapping (:EVDEV 11720 24834)
  :name "8BitDo SN30 Pro+"
  :icon-type :GENERIC-NINTENDO
  :buttons (305 :A 304 :B 308 :X 307 :Y 310 :L1 312 :L2 317 :L3 311 :R1 313 :R2
            318 :R3 314 :SELECT 306 :HOME 315 :START)
  :axes (0 :L-H 1 :L-V 2 :R-H 5 :R-V 16 :DPAD-H 17 :DPAD-V)
  :orientations (0 1.0 1 -1.0 2 1.0 5 -1.0 16 1.0 17 -1.0))

(define-device-mapping (:IOKIT 1118 736)
  :name "8BitDo SN30 Pro+"
  :icon-type :GENERIC-NINTENDO
  :buttons (589825 :A 589826 :B 589827 :X 589828 :Y 589829 :L1 589833 :L3
            589830 :R1 589834 :R3 589831 :SELECT 65669 :HOME 589832 :START)
  :axes (65587 :L2 65588 :R2 65584 :L-H 65585 :L-V 65586 :R-H 65589 :R-V 65650
         :DPAD-H 65651 :DPAD-V)
  :orientations (65584 1.0 65585 -1.0 65586 1.0 65589 -1.0))

(define-device-mapping (:IOKIT 1133 49674)
  :name "WingMan RumblePad"
  :icon-type :GENERIC-XBOX
  :buttons (589825 :A 589826 :B 589827 :C 589828 :X 589829 :Y 589830 :Z 589831
            :L1 589832 :R1 589833 :SELECT)
  :axes (65584 :L-H 65585 :L-V 65589 :R-H 65590 :R-V 65650 :DPAD-H 65651
         :DPAD-V 65586 :THROTTLE)
  :orientations NIL)

(define-device-mapping (:IOKIT 1356 616)
  :name "Sony DualShock 3"
  :icon-type :GENERIC-PLAYSTATION
  :buttons (589839 :A 589838 :B 589840 :X 589837 :Y 589835 :L1 589833 :L2
            589826 :L3 589836 :R1 589834 :R2 589827 :R3 589832 :DPAD-L 589830
            :DPAD-R 589829 :DPAD-U 589831 :DPAD-D 589825 :SELECT 589841 :HOME
            589828 :START)
  :axes (65584 :L-H 65585 :L-V 65586 :R-V 65589 :R-V)
  :orientations (65584 1.0 65585 -1.0 65586 1.0 65589 -1.0))

(define-device-mapping (:IOKIT 1356 1476)
  :name "Sony DualShock 4"
  :icon-type :DUALSHOCK-4
  :buttons (589826 :A 589827 :B 589825 :X 589828 :Y 589829 :L1 589831 :L2
            589835 :L3 589830 :R1 589832 :R2 589836 :R3 589833 :SELECT 589837
            :HOME 589834 :START)
  :axes (65587 :L2 65588 :R2 65584 :L-H 65585 :L-V 65586 :R-H 65589 :R-V 65650
         :DPAD-H 65651 :DPAD-V)
  :orientations NIL)

(define-device-mapping (:IOKIT 1356 2508)
  :name "Sony DualShock 4"
  :icon-type :DUALSHOCK-4
  :buttons (589826 :A 589827 :B 589825 :X 589828 :Y 589829 :L1 589831 :L2
            589835 :L3 589830 :R1 589832 :R2 589836 :R3 589833 :SELECT 589837
            :HOME 589834 :START)
  :axes (65587 :L2 65588 :R2 65584 :L-H 65585 :L-V 65586 :R-H 65589 :R-V 65650
         :DPAD-H 65651 :DPAD-V)
  :orientations NIL)

(define-device-mapping (:IOKIT 1356 3302)
  :name "Sony DualSense"
  :icon-type :DUALSHOCK-4
  :buttons (589826 :A 589827 :B 589825 :X 589828 :Y 589829 :L1 589831 :L2
            589835 :L3 589830 :R1 589832 :R2 589836 :R3 589833 :SELECT 589837
            :HOME 589834 :START)
  :axes (65587 :L2 65588 :R2 65584 :L-H 65585 :L-V 65586 :R-H 65589 :R-V 65650
         :DPAD-H 65651 :DPAD-V)
  :orientations NIL)

(define-device-mapping (:IOKIT 1356 3570)
  :name "Sony DualSense Edge"
  :icon-type :DUALSHOCK-4
  :buttons (589826 :A 589827 :B 589825 :X 589828 :Y 589829 :L1 589831 :L2
            589835 :L3 589830 :R1 589832 :R2 589836 :R3 589833 :SELECT 589837
            :HOME 589834 :START)
  :axes (65587 :L2 65588 :R2 65584 :L-H 65585 :L-V 65586 :R-H 65589 :R-V 65650
         :DPAD-H 65651 :DPAD-V)
  :orientations NIL)

(define-device-mapping (:IOKIT 1406 8201)
  :name "Pro Controller"
  :icon-type :NINTENDO-SWITCH
  :buttons (589825 :A 589826 :B 589827 :X 589828 :Y 589829 :L1 589831 :L2
            589835 :L3 589830 :R1 589832 :R2 589836 :R3 589833 :SELECT 589837
            :HOME 589834 :START)
  :axes (65584 :L-H 65585 :L-V 65587 :R-H 65588 :R-V 65650 :DPAD-H 65651
         :DPAD-V)
  :orientations (65584 1.0 65585 -1.0 65587 1.0 65588 -1.0 65650 1.0 65651 1.0))

(define-device-mapping (:IOKIT 1411 8288)
  :name "Buffalo BSGP801"
  :icon-type :GENERIC-NINTENDO
  :buttons (589826 :A 589825 :B 589828 :X 589827 :Y 589829 :L1 589830 :R1
            589831 :SELECT 589832 :START)
  :axes (65584 :DPAD-H 65585 :DPAD-V)
  :orientations (65584 1.0 65585 -1.0))

(define-device-mapping (:XINPUT 1118 767)
  :name "Xbox One"
  :icon-type :GENERIC-XBOX
  :buttons (0 :A 1 :B 3 :X 4 :Y 6 :L1 8 :L3 9 :R1 11 :R3 12 :DPAD-L 13 :DPAD-R
            14 :DPAD-U 15 :DPAD-D 16 :SELECT 18 :START)
  :axes (7 :L2 10 :R2 19 :L-H 20 :L-V 21 :R-H 22 :R-V)
  :orientations (7 1.0 10 1.0 19 1.0 20 1.0 21 1.0 22 1.0))

(define-device-mapping (:XINPUT 1133 49693)
  :name "Logitech F310"
  :icon-type :GENERIC-XBOX
  :buttons (0 :A 1 :B 3 :X 4 :Y 6 :L1 8 :L3 9 :R1 11 :R3 13 :DPAD-L 12 :DPAD-R
            14 :DPAD-U 15 :DPAD-D 16 :SELECT 18 :START)
  :axes (7 :L2 10 :R2 19 :L-H 20 :L-V 21 :R-H 22 :R-V)
  :orientations (7 1.0 10 1.0 19 1.0 20 1.0 21 1.0 22 1.0))

(define-device-mapping (:XINPUT 11720 12555)
  :name "8BitDo Ultimate 2"
  :icon-type :GENERIC-XBOX
  :buttons (0 :A 1 :B 3 :X 4 :Y 6 :L1 8 :L3 9 :R1 11 :R3 12 :DPAD-L 13 :DPAD-R
            14 :DPAD-U 15 :DPAD-D 16 :SELECT 18 :START)
  :axes (7 :L2 10 :R2 20 :L-H 21 :L-V 22 :R-H 23 :R-V)
  :orientations (7 1.0 10 1.0 20 1.0 21 1.0 22 1.0 23 1.0))

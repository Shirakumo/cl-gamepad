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

(define-device-mapping (:DINPUT 1356 2508)
  :name "Sony Dual Shock 4"
  :buttons (1 :A 2 :B 0 :X 3 :Y 4 :L1 6 :L2 10 :L3 5 :R1 7 :R2 11 :R3 8 :SELECT
            12 :HOME 9 :START)
  :axes (3 :L2 4 :R2 0 :L-H 1 :L-V 2 :R-H 5 :R-V 8 :DPAD-V))

(define-device-mapping (:DINPUT 1411 8288)
  :name "Buffalo BSGP801"
  :buttons (1 :A 0 :B 3 :X 2 :Y 4 :L1 5 :R1 6 :SELECT 7 :START)
  :axes (0 :DPAD-H 1 :DPAD-V))

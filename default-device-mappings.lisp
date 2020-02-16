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

(define-device-mapping (:DINPUT 1411 8288)
  :name "USB,2-axis 8-button gamepad  "
  :buttons (1 :A 0 :B 3 :X 2 :Y 4 :L1 5 :R1 6 :SELECT 7 :START)
  :axes (0 :DPAD-H 1 :DPAD-V))

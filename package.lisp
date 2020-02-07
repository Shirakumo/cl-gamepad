#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.gamepad
  (:use #:cl)
  (:export
   #:gamepad))

(defpackage #:org.shirakumo.fraf.gamepad.impl
  (:use #:cl)
  (:shadow #:byte)
  (:local-nicknames
   (#:gamepad #:org.shirakumo.fraf.gamepad)))

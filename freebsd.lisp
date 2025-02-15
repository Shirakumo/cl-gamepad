(in-package #:org.shirakumo.fraf.gamepad.impl)

(defun init ())

(defun shutdown ())

(defun list-devices ())

(defun call-with-devices (function))

(defun poll-devices (&key timeout function))

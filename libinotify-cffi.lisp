(in-package #:org.shirakumo.fraf.gamepad.impl)

(cffi:define-foreign-library libinotify
  (T (:or "libinotify.so" "libinotify.so.0" "libinotify.so.0.0.0")))

(cffi:use-foreign-library libinotify)


#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.impl)

(cffi:define-foreign-library corefoundation
  (T (:framework "CoreFoundation")))

(cffi:defcenum (run-loop-result :int32)
  (:finished 1)
  (:stopped 2)
  (:timed-out 3)
  (:handled-source 4))

(cffi:defcenum (number-type :uint32)
  (:int8      1)
  (:int16     2)
  (:int32     3)
  (:int64     4)
  (:float32   5)
  (:float64   6)
  (:char      7)
  (:short     8)
  (:int       9)
  (:long      10)
  (:long-long 11)
  (:float     12)
  (:double    13)
  (:index     14)
  (:integer   15)
  (:cg-float  16))

(cffi:defcfun (release "CFRelease") :void
  (object :pointer))

(cffi:defcfun (run-loop "CFRunLoopRunInMode") run-loop-result
  (mode :pointer)
  (seconds :double)
  (return-after-source-handled :bool))

(cffi:defcfun (create-number "CFNumberCreate") :pointer
  (allocator :pointer)
  (type number-type)
  (value :pointer))

(cffi:defcfun (create-dictionary "CFDictionaryCreate") :pointer
  (allocator :pointer)
  (keys :pointer)
  (values :pointer)
  (count :long)
  (key-callbacks :pointer)
  (value-callbacks :pointer))

(cffi:defcfun (create-array "CFArrayCreate") :pointer
  (allocator :pointer)
  (values :pointer)
  (count :long)
  (callbacks :pointer))

#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad.impl)

;;; Weak enums do not signal errors when translating from unknown integers,
;;; instead just returning the integer. This is useful if we are only interested
;;; in a subset of enum values, or need to accept user/vendor defined codes that
;;; we cannot know about in advance.

(defclass weak-foreign-enum (cffi::foreign-enum)
  ())

(defmethod cffi:translate-from-foreign (value (type weak-foreign-enum))
  (or (cffi::%foreign-enum-keyword type value :errorp NIL)
      value))

(defun make-weak-foreign-enum (type-name base-type values)
  (multiple-value-bind (base-type keyword-values value-keywords)
      (cffi::parse-foreign-enum-like type-name base-type values)
    (make-instance 'weak-foreign-enum
                   :name type-name
                   :actual-type (cffi::parse-type base-type)
                   :keyword-values keyword-values
                   :value-keywords value-keywords)))

(defmacro defcenum* (name-and-options &body enum-list)
  (cffi::%defcenum-like name-and-options enum-list 'make-weak-foreign-enum))

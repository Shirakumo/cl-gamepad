#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-gamepad
  :version "3.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Portability library for gamepad and joystick access."
  :homepage "https://Shirakumo.github.io/cl-gamepad/"
  :bug-tracker "https://github.com/Shirakumo/cl-gamepad/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-gamepad.git")
  :serial T
  :components ((:file "package")
               (:file "types")
               (:file "protocol")
               (:file "weak-enum")
               (:file "evdev-cffi" :if-feature :linux)
               (:file "linux" :if-feature :linux)
               (:file "win32-cffi" :if-feature :win32)
               (:file "xinput-cffi" :if-feature :win32)
               (:file "dinput-cffi" :if-feature :win32)
               (:file "windows" :if-feature :win32)
               (:file "corefoundation-cffi" :if-feature :darwin)
               (:file "iokit-cffi" :if-feature :darwin)
               (:file "darwin" :if-feature :darwin))
  :depends-on (:cffi
               :trivial-features
               :documentation-utils))

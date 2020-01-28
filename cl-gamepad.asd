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
               (:file "linux" :if-feature :linux))
  :depends-on (:cffi
               :trivial-features
               :documentation-utils))

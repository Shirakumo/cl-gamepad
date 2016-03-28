#|
 This file is a part of cl-gamepad
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem cl-gamepad-visualizer
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Gamepad visualizer to help figure out the proper IDs and test the library."
  :homepage "https://github.com/Shirakumo/cl-gamepad"
  :serial T
  :components ((:file "visualizer"))
  :depends-on (:cl-gamepad
               :qtools
               :qtcore
               :qtgui))

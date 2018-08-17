#|
 This file is a part of cl-gamepad
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-gamepad-visualizer
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Gamepad visualizer to help figure out the proper IDs and test the library."
  :homepage "https://Shirakumo.github.io/cl-gamepad/"
  :bug-tracker "https://github.com/Shirakumo/cl-gamepad/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-gamepad.git")
  :serial T
  :components ((:file "visualizer"))
  :depends-on (:cl-gamepad
               :qtools
               :qtcore
               :qtgui)
  :defsystem-depends-on (:qtools)
  :build-operation "qt-program-op"
  :build-pathname "gamepad-visualizer"
  :entry-point "cl-gamepad-visualizer:main")

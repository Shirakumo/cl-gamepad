#|
 This file is a part of cl-gamepad
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-gamepad
  :version "2.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Bindings to libstem_gamepad, allowing the handling of gamepad input."
  :homepage "https://Shirakumo.github.io/cl-gamepad/"
  :bug-tracker "https://github.com/Shirakumo/cl-gamepad/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-gamepad.git")
  :serial T
  :components ((:file "package")
               (:file "low-level")
               (:file "wrapper")
               (:file "documentation")
               #+linux (:file "database-lin")
               #+darwin (:file "database-mac")
               #+windows (:file "database-win"))
  :depends-on (:cffi
               :trivial-features
               :documentation-utils))

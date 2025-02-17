(asdf:defsystem cl-gamepad
  :version "3.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Portability library for gamepad and joystick access."
  :homepage "https://Shirakumo.github.io/cl-gamepad/"
  :bug-tracker "https://github.com/Shirakumo/cl-gamepad/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-gamepad.git")
  :build-operation "program-op"
  :build-pathname
  #+windows "gamepad-configurator.exe"
  #+linux "gamepad-configurator.run"
  #-(or windows linux) "gamepad-configurator.o"
  :entry-point "org.shirakumo.fraf.gamepad::configurator-main"
  :serial T
  :components ((:file "package")
               (:file "protocol")
               (:file "mapping")
               (:file "configurator")
               (:file "common")
               (:file "libinotify-cffi" :if-feature :bsd)
               (:file "evdev-cffi" :if-feature (:or :linux :bsd))
               (:file "linux" :if-feature (:or :linux :bsd))
               (:file "win32-cffi" :if-feature :windows)
               (:file "xinput-cffi" :if-feature :windows)
               (:file "dinput-cffi" :if-feature :windows)
               (:file "windows" :if-feature :windows)
               (:file "corefoundation-cffi" :if-feature :darwin)
               (:file "iokit-cffi" :if-feature :darwin)
               (:file "darwin" :if-feature :darwin)
               (:file "nx-cffi" :if-feature :nx)
               (:file "nx" :if-feature :nx)
               (:file "default-device-mappings")
               (:file "documentation"))
  :defsystem-depends-on (:trivial-features)
  :depends-on ((:feature (:or :linux :bsd :windows :darwin :nx) :cffi)
               (:feature :windows :com-on)
               :documentation-utils))

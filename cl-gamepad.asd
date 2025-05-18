(asdf:defsystem cl-gamepad
  :version "3.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Portability library for gamepad and joystick access."
  :homepage "https://Shirakumo.github.io/cl-gamepad/"
  :bug-tracker "https://github.com/Shirakumo/cl-gamepad/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-gamepad.git")
  :build-operation "deploy-console-op"
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
               (:file "evdev-cffi" :if-feature :linux)
               (:file "linux" :if-feature :linux)
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
  :defsystem-depends-on (:trivial-features :deploy)
  :depends-on ((:feature (:or :linux :windows :darwin :nx) :cffi)
               (:feature :windows :com-on)
               :documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :cl-gamepad/tests))))

(defsystem cl-gamepad/tests
  :serial t
  :components ((:file "package-test")
               (:file "linux-test" :if-feature :linux))
  :depends-on (:parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.fraf.gamepad.test)))

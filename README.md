## About cl-gamepad
This is a small wrapper library around [libstem_gamepad](https://github.com/Shirakumo/libstem_gamepad), providing independent gamepad input support.

## How To
Precompiled versions of the library are already provided in the repository for most major platforms. However, you may build your own version from the above repository if you so please.

Simply load cl-gamepad with ASDF or Quicklisp

    (ql:quickload :cl-gamepad)

Before you can handle any devices, you must call `init` in order to set the environment up and prepare everything. If you would like to use a notification based mechanism, you should also define overrides for the `device-attached`, `device-removed`, `button-pressed`, `button-released`, and `axis-moved` functions. For illustrative purposes we will do so just for two of those here:

    (defun cl-gamepad:device-attached (device)
      (format T "~&Found new device: ~a" device))
    
    (defun cl-gamepad:device-removed (device)
      (format T "~&Device removed: ~a" device))
    
    (cl-gamepad:init)

Next you need to call `detect-devices` whenever your application is ready to add new devices (this may or may not be always) and call `process-events` in your game/event/whatever loop to receive and process events. For simplicity's sake we will just call both in a loop here.

    (loop (cl-gamepad:detect-devices)
          (cl-gamepad:process-events)
          (format T "~&Device status: ~&~a" (mapcar #'cl-gamepad:device-plist (cl-gamepad:devices)))
          (sleep 0.5))

During shut down of your application --or whenever you no longer want to receive input-- you should call `shutdown` to clean everything up.

    (cl-gamepad:shutdown)

## Visualizer
Cl-gamepad includes a little visualizer Qt application that lets you view the complete controller state graphically. You can use this to test the library, your controller, or figure out the ID mapping of the buttons and axis.

    (ql:quickload :cl-gamepad-visualizer)
    (cl-gamepad-visualizer:main)

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

Note that the button and axis IDs you receive in the callback are just numbers and may differ between controllers and platforms. In order to help standardise this, cl-gamepad includes a device-map functionality. Using `axis-label`, `axis-multiplier`, and `button-label`, the various features can be normalised. See the corresponding docstrings for the defined set of labels and what they mean.

Unfortunately the mapping is not automatable and the current database of known devices included in cl-gamepad is rather small. It is thus very well possible that the functions will just return `:unknown` instead. However, using the visualizer below you can figure out that mapping and define it yourself. If you have a controller that isn't in the database yet, please take the time to map it out and send the definition to us, either as a [PR](https://github.com/Shirakumo/cl-gamepad/compare) or as an [issue](https://github.com/Shirakumo/cl-gamepad/issues/new).

You can view and set the complete mapping for a given controller via `device-map`. It takes the controller's vendor and product IDs to identify the controller kind. Usually however you'll want a more convenient way to define a mapping, which you can do through `define-gamepad`. An example mapping for the [Buffalo BSGP801](http://buffalo.jp/product/input/gamepad/bsgp801/) controller on Linux looks like this:

    (define-gamepad buffalo-bsgp801 (1411 8288)
      (:buttons
       ( 0 :b)
       ( 1 :a)
       ( 2 :x)
       ( 3 :y)
       ( 4 :l1)
       ( 5 :r1)
       ( 6 :select)
       ( 7 :start))
      (:axes
       ( 0 :dpad-h)
       ( 1 :dpad-v)))

Finally, during shut down of your application -- or whenever you no longer want to receive input -- you should call `shutdown` to clean everything up.

    (cl-gamepad:shutdown)

## Visualizer
Cl-gamepad includes a little visualizer Qt application that lets you view the complete controller state graphically. You can use this to test the library, your controller, or figure out the ID mapping of the buttons and axis.

    (ql:quickload :cl-gamepad-visualizer)
    (cl-gamepad-visualizer:main)

By clicking on an axis or button you can assign the label you deem fitting to it. Once all the buttons and axes have been assigned, you can click on the export button to get a nicely formatted definition form that you can use to persist the configuration in code.

#|
 This file is a part of cl-gamepad
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad)

;; low-level.lisp
(docs:define-docs
  (variable *here*
    "Variable containing the path to this very file, hopefully.")

  (variable *static*
    "Variable containing a pathname to the static directory.")

  (type device
    "C struct that contains all relevant information about a gamepad device.

You should never SETF any of its fields unless you really 
know what you're doing. Passing a pointer to this struct
outside of the dynamic-extent in which it was provided will
cause undefined behaviour and even lead as far as crashing
your program. If you need to pass it around, you should use
DEVICE-PLIST to serialise it.

See ID
See VENDOR
See PRODUCT
See DESCRIPTION
See AXIS-COUNT
See AXIS
See AXES
See BUTTON-COUNT
See BUTTON
See BUTTONS
See DEVICE-PLIST

See DEVICE-ID
See DEVICE-VENDOR
See DEVICE-PRODUCT
See DEVICE-DESCRIPTION
See DEVICE-AXIS-COUNT
See DEVICE-AXIS-STATES
See DEVICE-BUTTON-COUNT
See DEVICE-BUTTON-STATES
See DEVICE-PRIVATE-DATA
See DETECT-DEVICES")
  
  (function device-id
    "Accesses the unique identifier for the gamepad device.

Starts at 0 and increases for each new gamepad that is detected.
If a particular gamepad is disconnected and later reconnected,
it will receive a different id.

See DEVICE")
  
  (function device-description
    "Accesses the description string for the gamepad device.

See DEVICE")
  
  (function device-vendor
    "Accesses the vendor ID for the gamepad device.

This along with DEVICE-PRODUCT may be useful to detect default
behaviour for a particular controller.

See DEVICE")
  
  (function device-product
    "Accesses the product ID for the gamepad device.

This along with DEVICE-VENDOR may be useful to detect default
behaviour for a particular controller.

See DEVICE")
  
  (function device-axis-count
    "Accesses the number of axes the controller seems to have.

See DEVICE")
  
  (function device-button-count
    "Accesses the number of buttons the controller seems to have.

See DEVICE")
  
  (function device-axis-states
    "Accesses the pointer to the float array that describes the state of the various axes.

See DEVICE
See DEVICE-AXIS")
  
  (function device-button-states
    "Accesses the pointer to the bool array that describes the state of the various buttons.

See DEVICE
See DEVICE-BUTTON")
  
  (function device-private-data
    "Accesses the pointer to the private data of the gamepad device struct.

You should never need this.

See DEVICE"))

;; wrapper.lisp
(docs:define-docs
  (type index-out-of-range
    "Condition signalled when an index is out of its allowed range.

See INDEX
See RANGE")

  (function index
    "Reader to the index that the condition happened on.

See INDEX-OUT-OF-RANGE")

  (function range
    "Reader to the range that the condition happened on.

See INDEX-OUT-OF-RANGE")
  
  (function id
    "Returns the unique identifier for the gamepad device.

Starts at 0 and increases for each new gamepad that is detected.
If a particular gamepad is disconnected and later reconnected,
it will receive a different id.

See DEVICE")
  
  (function description
    "Returns the description string for the gamepad device.

See DEVICE")
  
  (function vendor
    "Returns the vendor ID for the gamepad device.

This along with DEVICE-PRODUCT may be useful to detect default
behaviour for a particular controller.

See DEVICE")
  
  (function product
    "Returns the product ID for the gamepad device.

This along with DEVICE-VENDOR may be useful to detect default
behaviour for a particular controller.

See DEVICE")
  
  (function axis-count
    "Returns the number of axes the controller seems to have.

See DEVICE")
  
  (function button-count
    "Returns the number of buttons the controller seems to have.

See DEVICE")
  
  (function axis
    "Return the current state of a particular axis on the gamepad device.

The axis should be a number from 0 below DEVICE-AXIS-COUNT.
If an invalid axis id is passed, an INDEX-OUT-OF-RANGE error is
signalled.
Returned is a float in the range [-1 1].

See DEVICE
See AXIS-COUNT")

  (function axes
    "Returns a vector of the state of each axis on the gamepad device.

See DEVICE
See AXIS")

  (function button
    "Return the current state of a particular button on the gamepad device.

The button should be a number from 0 below DEVICE-BUTTON-COUNT.
If an invalid button id is passed, an INDEX-OUT-OF-RANGE error is
signalled.
Returned is T for pressed and NIL for released.

See DEVICE
See DEVICE-BUTTON-COUNT")

  (function buttons
    "Returns a vector of the state of each button on the gamepad device.

See DEVICE
See BUTTON")

  (function device-plist
    "Constructs a plist from the device's values.

Returned is:
(:ID id
 :VENDOR vendor
 :PRODUCT product 
 :DESCRIPTION description
 :AXES axes
 :BUTTONS buttons)

See ID
See VENDOR
See PRODUCT
See DESCRIPTION
See AXES
See BUTTONS
See DEVICE")
  
  (function device-attached
    "This function is called whenever a new device is attached.

You should override this function if you want to use a
notification mechanism. By default this function does nothing.

This function is called in the same thread as the DETECT-DEVICES
call occurs that triggers it and may thus block it.

See DEVICE
See DETECT-DEVICES")
  
  (function device-removed
    "This function is called whenever a device is removed.

You should override this function if you want to use a 
notification mechanism. By default this function does nothing.

This function is called in the same thread as the PROCESS-EVENTS
call occurs that triggers it and may thus block it.

See DEVICE
See PROCESS-EVENTS")
  
  (function button-pressed
    "This function is called whenever a button is pressed.

BUTTON is an integer identifying the button on the controller.
TIME is a float representing the time at which the movement
occurred precisely.

You should override this function if you want to use a 
notification mechanism. By default this function does nothing.

This function is called in the same thread as the DETECT-DEVICES
or PROCESS-EVENTS call occurs that triggers it and may thus block
it.

See DEVICE
See DETECT-DEVICES
See PROCESS-EVENTS")
  
  (function button-released
    "This function is called whenever a button is released.

BUTTON is an integer identifying the button on the controller.
TIME is a float representing the time at which the movement
occurred precisely.

You should override this function if you want to use a 
notification mechanism. By default this function does nothing.

This function is called in the same thread as the DETECT-DEVICES
or PROCESS-EVENTS call occurs that triggers it and may thus block
it.

See DEVICE
See DETECT-DEVICES
See PROCESS-EVENTS")
  
  (function axis-moved
    "This function is called whenever an axis is moved.

AXIS is an integer identifying the axis on the controller.
LAST-VALUE is a float representing the value of this axis at
the previous time AXIS-MOVED was called.
VALUE is a float representing the current value of this axis.
TIME is a float representing the time at which the movement
occurred precisely.

LAST-VALUE and VALUE are in the range of [-1 1].

You should override this function if you want to use a 
notification mechanism. By default this function does nothing.

This function is called in the same thread as the DETECT-DEVICES
or PROCESS-EVENTS call occurs that triggers it and may thus block
it.

See DEVICE
See DETECT-DEVICES
See PROCESS-EVENTS")
  
  (function init
    "Sets everything up and prepares the library to process input events.

This function should only ever be called once until a matching
SHUTDOWN call has occurred. This function must be called before
any of the other functions are called.

See SHUTDOWN")
  
  (function shutdown
    "Cleans everything up so that it's back to being tidy and neat.

This function should only ever be called once and only after a
matching INIT call has occurred. No other functions but INIT
should ever be called after this function has been called.

See INIT")
  
  (function device-count
    "Returns an integer of how many devices we know of right now.")
  
  (function device
    "Returns the device at the specified index.

The index should be a number from 0 below DEVICE-COUNT.
Passing an invalid number results in undefined behaviour, going
as far as potentially crashing your program.

See DEVICE")

  (function devices
    "Returns a list of all known devices.

See DEVICE-COUNT
See DEVICE")
  
  (function detect-devices
    "Polls for new devices.

May call DEVICE-ATTACHED BUTTON-PRESSED BUTTON-RELEASED AXIS-MOVED

New devices will not be noticed unless and until you call this
function. As such you should call it at any time it is appropriate
for new controllers to be introduced to your program.")
  
  (function process-events
    "Processes input events from all known controllers.

May call DEVICE-REMOVED BUTTON-PRESSED BUTTON-RELEASED AXIS-MOVED

If a device is unplugged, you will not be notified of this until
this function is called. The corresponding DEVICE-REMOVED function
is then called. If a new device is plugged in, events for it will
not be processed until it is registered through DETECT-DEVICES.

See DETECT-DEVICES"))

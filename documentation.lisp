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

  (variable DEVICE-MAP-MAX
    "Returns the maximum ID that can be referenced from a device-map.")

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
See AXIS-LABEL
See AXIS-MULTIPLIER
See AXES
See BUTTON-COUNT
See BUTTON
See BUTTON-LABEL
See BUTTONS
See DEVICE-PLIST
See DEVICE-MAP
See UPDATE-DEVICE-MAP

See DEVICE-ID
See DEVICE-VENDOR
See DEVICE-PRODUCT
See DEVICE-DESCRIPTION
See DEVICE-AXIS-COUNT
See DEVICE-AXIS-STATES
See DEVICE-BUTTON-COUNT
See DEVICE-BUTTON-STATES
See DEVICE-DEVICE-MAP
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
Note that the bools are not strict in the sense that anything greater than 0
should be taken as TRUE.

See DEVICE
See DEVICE-BUTTON")

  (function device-device-map
    "Accesses the pointer to the device map that describes the button and axis layout.

See DEVICE
See DEVICE-MAP")
  
  (function device-private-data
    "Accesses the pointer to the private data of the gamepad device struct.

You should never need this.

See DEVICE")

  (type device-map
    "Contains information to map physical buttons and axes to standardised labels relating to their function.

The map contains three particular mappings, limited to at most
a physical ID of DEVICE-MAP-MAX minus one.

  button map       --- Each slot maps to a BUTTON enum value that
                       describes the button's function.
  axis map         --- Each slot maps to an AXIS enum value that
                       describes the axis' function.
  axis multiplier  --- Each slot maps  to a char of either 1 or -1
                       representing a multiplier to standardise the
                       axis' direction.

Note that the functionality of the buttons and axes should be
normalised based on the layout of an XBOX 360 controller.

See DEVICE-MAP-MAX
See BUTTON
See AXIS
See DEVICE-MAP-BUTTON-MAP
See DEVICE-MAP-BUTTONS
See DEVICE-MAP-AXIS-MAP
See DEVICE-MAP-AXES
See DEVICE-MAP-AXIS-MULTIPLIER
See DEVICE-MAP-AXIS-MULTIPLIERS")

  (function device-map-button-map
    "Accessor to the first button mapping of the device-map.

The button map is an in-struct array, so you'll probably want to
use DEVICE-MAP-BUTTONS instead.

See DEVICE-MAP
See DEVICE-MAP-BUTTONS")

  (function device-map-buttons
    "Accessor to the C array of the button mappings.

See DEVICE-MAP")

  (function device-map-axis-map
    "Accessor to the first axis mapping of the device-map.

The axis map is an in-struct array, so you'll probably want to
use DEVICE-MAP-AXES instead.

See DEVICE-MAP
See DEVICE-MAP-AXES")

  (function device-map-axes
    "Accessor to the C array of the axis mappings.

See DEVICE-MAP")

  (function device-map-axis-multiplier
    "Accessor to the first axis multiplier of the device-map.

The multiplier map is an in-struct array, so you'll probably want to
use DEVICE-MAP-AXIS-MULTIPLIERS instead.

See DEVICE-MAP
See DEVICE-MAP-AXIS-MULTIPLIERS")

  (function device-map-axis-multipliers
    "Accessor to the C array of the axis multipliers.

See DEVICE-MAP")

  (function gamepad-init
    "Calls the Gamepad_init C function to initialize the library.")

  (function gamepad-shutdown
    "Calls the Gamepad_shutdown C function to clean up the library.")

  (function gamepad-num-devices
    "Calls the Gamepad_numDevices C function, returning the number of devices currently known to the library.")

  (function gamepad-device-at-index
    "Calls the Gamepad_deviceAtIndex C function, returning a device pointer corresponding to the index.")

  (function gamepad-device-map
    "Retrieves the device map for the given vendor and product. If no specific map is known, a generic one is returned.")

  (function gamepad-set-device-map
    "Set the device map for a given vendor and product. The struct's contents are copied, and you should free the one you pass in yourself.")
  
  (function gamepad-detect-devices
    "Calls the Gamepad_detectDevices C function, leading to new devices being recognised.")
  
  (function gamepad-process-events
    "Calls the Gamepad_processEvents C function, leading to input events being captured and the callbacks being invoked.")

  (function gamepad-device-attach-func
    "Calls the Gamepad_deviceAttachFunc C function, registering the callback function.")

  (function gamepad-device-remove-func
    "Calls the Gamepad_deviceRemoveFunc C function, registering the callback function.")

  (function gamepad-button-down-func
    "Calls the Gamepad_buttonDownFunc C function, registering the callback function.")

  (function gamepad-button-up-func
    "Calls the Gamepad_buttonUpFunc C function, registering the callback function.")

  (function gamepad-axis-move-func
    "Calls the Gamepad_axisMoveFunc C function, registering the callback function."))

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
The axis direction is normalised per the axis multiplier in the
corresponding device map.

See DEVICE
See AXIS-COUNT")

  (function axis-label
    "Return the label for the given physical axis on the gamepad device.

The axis should be a number from 0 below DEVICE-AXIS-COUNT.
If an invalid axis id is passed, :UNKNOWN is returned.
Returned is a keyword out of the following set:

  :unknown     --- An unknown axis. The default.
  :l-h         --- The horizontal movement of the left analog stick.
  :l-v         --- The vertical movement of the right analog stick.
  :r-h         --- The horizontal movement of the right analog stick.
  :r-v         --- The vertical movement of the right analog stick-
  :dpad-h      --- The horizontal movement of the dpad.
  :dpad-v      --- The vertical movement of the dpad.
  :button-x    --- The amount the X (left)  button is pressed.
  :button-y    --- The amount the Y (up)    button is pressed.
  :button-z    --- The amount the Z         button is pressed.
  :button-a    --- The amount the A (down)  button is pressed.
  :button-b    --- The amount the B (right) button is pressed.
  :button-c    --- The amount the C         button is pressed.
  :l1          --- The amount the left upper trigger is pressed.
  :l2          --- The amount the left lower trigger is pressed.
  :r1          --- The amount the right upper trigger is pressed.
  :r2          --- The amount the right lower trigger is pressed.
  :tilt-x      --- The tilting of the controller in X direction.
  :tilt-y      --- The tilting of the controller in Y direction.
  :tilt-z      --- The tilting of the controller in Z direction.
  :move-x      --- The acceleration in X direction.
  :move-y      --- The acceleration in Y direction.
  :move-z      --- The acceleration in Z direction.
  :wheel       --- The turning of the wheel.
  :accelerator --- The amount the accelerator is pressed.
  :brake       --- The amount the brake is pressed.
  :x           --- The movement of the joystick in X direction.
  :y           --- The movement of the joystick in Y direction.
  :z           --- The rotation of the joystick in Z direction.
  :throttle    --- The value of the throttle.

The button map is mostly modelled after the XBOX360 controller.

See DEVICE
See DEVICE-MAP")

  (function axis-multiplier
    "Return the multiplier for the axis value.

Returns either 1 or -1 depending on whether the value of the
axis should be inverted or not.
The axis should be a number from 0 below DEVICE-AXIS-COUNT.
If an invalid axis id is passed, 1 is returned.

See DEVICE
See DEVICE-MAP")

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

  (function button-label
            "Return the label for the given physical button on the gamepad device.

The button should be a number from 0 below DEVICE-BUTTON-COUNT.
If an invalid button id is passed, :UNKNOWN is returned.
Returned is a keyword out of the following set:

  :unknown --- An unknown button. This is the default.
  :x       --- The X (left)  button.
  :y       --- The Y (up)    button.
  :z       --- The Z         button.
  :a       --- The A (down)  button.
  :b       --- The B (right) button.
  :c       --- The C         button.
  :l1      --- The left upper trigger.
  :l2      --- The left lower trigger.
  :r1      --- The right upper trigger.
  :r2      --- The right lower trigger.
  :l       --- Whether the left analog stick is pressed in.
  :r       --- Whether the right analog stick is pressed in.
  :dpad-l  --- The left direction of the dpad.
  :dpad-r  --- The right direction of the dpad.
  :dpad-u  --- The up direction of the dpad.
  :dpad-d  --- The down direction of the dpad.
  :select  --- The select (left) button.
  :home    --- The home (center) button.
  :start   --- The start (right) button.
  :trigger --- The trigger button.

The button map is mostly modelled after the XBOX360 controller.

See DEVICE
See DEVICE-MAP")

  (function buttons
    "Returns a vector of the state of each button on the gamepad device.

See DEVICE
See BUTTON")

  (function device-plist
    "Constructs a plist from the device's values.

Returned is:
\(:ID id
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

Note that the values are NOT multiplied by the axis-multiplier
defined in the corresponding device map. You are responsible for
normalising the direction yourself.

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

This function must be called before any of the other functions
are called. It is safe to call this function any number of times
in sequence, but only one \"actual\" init is performed before
another SHUTDOWN.

See SHUTDOWN")
  
  (function shutdown
    "Cleans everything up so that it's back to being tidy and neat.

No other functions but INIT should ever be called after this
function has been called. It is safe to call this function any
number of times in sequence, but only one \"actual\" shutdown
is performed before another INIT.

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

See DETECT-DEVICES")

  (function print-device
    "Prints an unreadable representation of DEVICE to STREAM.

Useful for debugging prints.

See DEVICE")

  (function device-map
    "Accessor to the device map for the given vendor and product.

The device map assigns labels to the buttons and axes and normalises
the axes' direction if necessary. The map is a list of the following
structure:

   DEVICE-MAP ::= (BUTTON-MAP AXIS-MAP)
   BUTTON-MAP ::= (:buttons BUTTON*)
   BUTTON     ::= (ID LABEL)
   AXIS-MAP   ::= (:axes AXIS*)
   AXIS       ::= (ID LABEL MULTIPLIER)
   ID         --- A numerical ID of a button or axis
   LABEL      --- The keyword label for the button or axis' functionality.
   MULTIPLIER --- Either 1 or -1 to potentially reverse the axis' direction.

See AXIS-LABEL and BUTTON-LABEL for the available labels and their
intended usage. A sample map for the Xbox360 controller as recognised
on Linux platforms follows.

   ((:axes
     ( 0 :l-h)
     ( 1 :l-v)
     ( 2 :l2)
     ( 3 :r-h)
     ( 4 :r-v)
     ( 5 :r2)
     ( 6 :dpad-h)
     ( 7 :dpad-v))
    (:buttons
     ( 0 :a)
     ( 1 :b)
     ( 2 :x)
     ( 3 :y)
     ( 4 :l1)
     ( 5 :r1)
     ( 6 :select)
     ( 7 :start)
     ( 8 :home)
     ( 9 :l)
     (10 :r)))

Note that the IDs for each button and axis are unfortunately very
much platform-dependent and a mapping for each platform must be
created individually.")

  (function update-device-map
    "Update the device map for a select few records.

Unlike (SETF DEVICE-MAP) this does not clear out the mappings that
are not explicitly given, and instead leaves those to their default
values.

See DEVICE-MAP")

  (function define-gamepad
    "Define the mapping for a particular gamepad device.

The MAP should follow the same schema as the one defined in the
DEVICE-MAP docstring. if the INHERIT option is passed, the defined
mappings are merged together with those of the inherited gamepad
mapping definition. This allows you to easily define gamepads that
are otherwise very similar.

See DEVICE-MAP
See GAMEPAD-DEFINITION")

  (function gamepad-definition
    "Returns a string representation of a valid mapping definition for the given device.

This function is useful if you want to extract the available device
mapping information to turn it into a static gamepad definition as
used by DEFINE-GAMEPAD.

See DEFINE-GAMEPAD"))

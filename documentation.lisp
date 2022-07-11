#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad)

;;; configurator.lisp
(docs:define-docs
  (function configure-device
    "Run an interactive configuration wizard to determine the proper button mappings.

This will interactively ask you to press the correct buttons and axes
corresponding to the prompted labels.

You can use this to configure a device's button and axis mappings if
the default mapping is not correct.

You can pass the desired axis and button labels.

If the device reports faulty axes that interfere with the axis mapping
mechanism, you can ignore them by passing their IDs with the IGNORED-AXES
argument.

See +COMMON-BUTTONS+
See +COMMON-AXES+
See SAVE-DEVICE-MAPPINGS"))

;;; mapping.lisp
(docs:define-docs
  (type *default-mappings-file*
    "The file used to store device mapping configuration in.

This file is loaded when the library is loaded.

See SAVE-DEVICE-MAPPINGS")

  (function blacklisted-p
    "A place accessing whether the device is blacklisted or not.

If T, the device will not be recognised on future initialisations
of the library or future POLL-DEVICES calls.")
  
  (function device-mapping
    "A place accessing the device mapping associated with the given ID.

An ID can either be a DEVICE instance, or a list with the following
members:

  DRIVER   --- The name of the underlying driver for the device.
               One of :DINPUT :XINPUT :EVDEV :IOKIT
  VENDOR   --- The vendor ID of the device.
  PRODUCT  --- The product ID of the device.

Returns NIL if no mapping is known, or a plist with the following
members:

  :NAME    --- A string identifying the device in a hopefully
               human-readable way.
  :BUTTONS --- A hash table mapping backend-specific button IDs to the
               standardised button labels from +LABELS+
  :AXES    --- A hash table mapping backend-specific axis IDs to the
               standardised axis labels from +LABELS+

Setting a device mapping will automatically update all known
devices that match the mapping.

See REMOVE-DEVICE-MAPPING
See CONFIGURE-DEVICE")
  
  (function remove-device-mapping
    "Removes the device mapping associated with the given ID.

See DEVICE-MAPPING")
  
  (function define-device-mapping
    "Defines a new device mapping for the named device type.

This is a shorthand around (SETF DEVICE-MAPPING). The BODY should be a
plist with :NAME :BUTTONS and :AXES keys. For buttons and axes, the
value should be a plist from device IDs to labels.

See DEVICE-MAPPING")
  
  (function save-device-mappings
    "Saves all known device mappings to a lisp source file.

The source file can simply be LOADed in to restore the mappings.

See *DEFAULT-MAPPINGS-FILE*"))

;;; protocol.lisp
(docs:define-docs
  (variable +labels+
    "Vector of standardised button and axis labels.

Modifying this value results in undefined behaviour.

The labels are intended to have the following semantic meanings:

  A        --- The A B C buttons are laid out on the bottom diagonal
  B            named button row, with A being the bottom-most.
  C            This follows the \"Xbox\" convention.
  X        --- The X Y Z buttons are laid out on the top diagonal
  Y            named button row, with X being the bottom-most.
  Z            This follows the \"Xbox\" convention.
  L1       --- The left hand upper trigger button.
  L2       --- The left hand lower trigger button/axis.
  L3       --- The left hand stick press-in button.
  R1       --- The right hand upper trigger button.
  R2       --- The right hand lower trigger button/axis.
  R3       --- The right hand stick press-in button.
  DPAD-L   --- The left dpad button.
  DPAD-R   --- The right dpad button.
  DPAD-U   --- The upper dpad button.
  DPAD-D   --- The lower dpad button.
  SELECT   --- The left central button. Often named select, back, or share.
  HOME     --- The middle central button. Usually a logo.
  START    --- The right central button. Often named start, or options.
  L-H      --- The left stick horizontal axis.
  L-V      --- The left stick vertical axis.
  R-H      --- The right stick horizontal axis.
  R-V      --- The right stick vertical axis.
  DPAD-H   --- The dpad horizontal axis.
  DPAD-V   --- The dpad vertical axis.
  TILT-X   --- The tilt degree around the X (along the length) axis.
  TILT-Y   --- The tilt degree around the Y (along the height) axis.
  TILT-Z   --- The tilt degree around the Z (along the depth) axis.
  MOVE-X   --- The relative motion over the X axis.
  MOVE-Y   --- The relative motion over the Y axis.
  MOVE-Z   --- The relative motion over the Z axis.
  WHEEL    --- The steering wheel direction.
  GAS      --- The gas pedal.
  BRAKE    --- The brake pedal.
  THROTTLE --- The throttle strength.
  RUDDER   --- The rudder direction.
")
  
  (variable +common-buttons+
    "A vector of the most commonly found button labels.

See +LABELS+
See CONFIGURE-DEVICE")
  
  (variable +common-axes+
    "A vector of the most commonly found axis labels.

See +LABELS+
See CONFIGURE-DEVICE")

  (type gamepad-error
    "Error signalled when an underlying operating system, driver, or device problem occurs.

Depending on the driver used the actual type of error signalled may be
a subtype of this condition with more information available about the
error specifics.")
  
  (type event
    "Base type for device events.

An event denotes a change in the device's buttons or axes.

See POLL-EVENTS
See BUTTON-DOWN
See BUTTON-UP
See AXIS-MOVE
See EVENT-DEVICE
See EVENT-TIME
See EVENT-CODE
See EVENT-LABEL
See EVENT-VALUE
See EVENT-OLD-VALUE")
  
  (function event-device
    "Returns the device the event came from.

This field is always set.

See EVENT
See DEVICE")
  
  (function event-time
    "Returns some form of time identifier for the event.

The actual time resolution is dependent on the driver. This is mostly
useful to gauge whether events happened simultaneously or not.

This field is always set.

See EVENT")
  
  (function event-code
    "Returns the underlying code of the feature on the device that caused the event.

The code is a positive integer that has a device and driver specific
meaning.

This field is always set.

See EVENT")
  
  (function event-label
    "Returns the standardised label associated with the axis or button that caused the event.

If the feature is unknown or not properly mapped, NIL is
returned. Otherwise the returned symbol must be one contained in
+LABELS+.

See EVENT
See +LABELS+")
  
  (function event-value
    "Returns the axis value as a single-float in [-1,+1].

For a horizontal axis, -1 means left, +1 means right.
For a vertical axis, -1 means down, +1 means up.

This field is only accessible for AXIS-MOVEs.

See AXIS-MOVE")

  (function event-old-value
    "Returns the previous axis value as a single-float in [-1,+1]

See EVENT-VALUE
See AXIS-MOVE")
  
  (type button-down
    "Event issued when a button is pressed down.

This event is only signalled once on the rising edge.

See EVENT")
  
  (type button-up
    "Event issued when a button is released.

This event is only signalled once on the falling edge.

See EVENT")
  
  (type axis-move
    "Event issued when an axis moves.

This event may be signalled many times with very similar values.
It will NOT be issued if the combined axis (horizontal and vertical)
has a dead zone set and the axis falls within the dead zone.
If a ramp is defined on the axis, the value is already ramp-adjusted.

See EVENT-VALUE
see EVENT-OLD-VALUE
See EVENT")
  
  (type device
    "Representation of a physical gamepad device.

The actual type of instances may be a subtype of this with internal
features relevant to the driver.

See NAME
See VENDOR
See PRODUCT
See VERSION
See DRIVER
See ICON-TYPE")
  
  (function name
    "Returns a human-readable name for the device.

No guarantees can be made about the quality or descriptiveness of the
name.

See DEVICE")
  
  (function vendor
    "Returns an integer ID identifying the vendor of the device.

See DEVICE")
  
  (function product
    "Returns an integer ID identifying the device product.

See DEVICE")
  
  (function version
    "Returns an integer denoting the version of the device.

See DEVICE")
  
  (function driver
    "Returns a symbol identifying the driver underlying the device.

May be one of:

  :EVDEV   --- (Linux evdev)
  :DINPUT  --- (Windows DirectInput)
  :XINPUT  --- (Windows XInput)
  :IOKIT   --- (Apple IOKit)

See DEVICE")

  (function icon-type
   "Returns a symbol identifying what icons to use for the buttons.

May be one of:

  :GENERIC-NINTENDO
  :GENERIC-XBOX
  :GENERIC-PLAYSTATION
  :NINTENDO-SWITCH
  :DUALSHOCK-4

or another kind of label. You should be prepared to default to
:GENERIC-XBOX on encountering an unknown icon type.

See DEVICE")

  (function button
    "Returns the last known value of the button.

Returns T if the button is pressed, NIL if it is released.
This state is automatically updated when POLL-EVENTS is called.
The button name must be a known and mapped keyword from +LABELS+.

See BUTTON-DOWN
See BUTTON-UP
See POLL-EVENTS
See DEVICE
See +LABELS+")

  (function axis
    "Returns the last known value of the axis.

Returns a single-float representing the axis state.
This state is automatically updated when POLL-EVENTS is called.
The axis name must be a known and mapped keyword from +LABELS+.

See AXIS-MOVE
See POLL-EVENTS
See DEVICE
See +LABELS+")
  
  (function dead-zone
    "Accessor for the dead zone of the axis on the device.

The dead zone can be defined on a combination of horizontal and
vertical axes, or a single axis. When applied to a combination, the
axis name must be :L, or :R, in which case the dead zone is circular.

See +LABELS+
See DEVICE")
  
  (function ramp
    "Accessor for the ramp of the axis on the device.

The ramp descriptor must be a function of a single argument that takes
a basic, linear axis value, and returns the mapped axis value.

See +LABELS+
See DEVICE")
  
  (function init
    "Initialises the library for use.

This will cause system shared objects to be loaded and will cause
certain support structures to be allocated and initialised. Only call
this function on a target system where the gamepads should be queried.

You MUST call this function before calling any other device functions.

It is safe to call this function multiple times.

Returns the list of currently known devices.

See SHUTDOWN")
  
  (function shutdown
    "Uninitialises the library and closes all used resources.

This will not unload system shared objects, but it will close all used
devices and other services and deallocate them. You should call this
function when shutting down on a target system.

After calling this function you MUST NOT call any other device
functions without first calling INIT again.

It is safe to call this function multiple times.

See INIT")
  
  (function list-devices
    "Returns a fresh list of all known gamepad devices.

This will not query for device updates, and only represents the
currently known state of the system.

On an uninitialised system this will return NIL.

See POLL-DEVICES")
  
  (function poll-devices
    "Queries for device changes.

TIMEOUT can be a one of the following:

   T    --- Continuously poll for device changes indefinitely.
   NIL  --- Immediately return if no changes are pending.
   REAL --- Wait up to the given number of seconds until a change is
            noticed. Returns as soon as a change happened, or some
            time until the timeout runs out.

You should call this function whenever you want to allow changing the
connected devices, or whenever an existing device is disconnected.

You must have called INIT prior to calling this function.

See INIT
See LIST-DEVICES")
  
  (function poll-events
    "Queries the device for events.

The FUNCTION is called with any new device events that arrive during
querying. The events are not guaranteed to be fresh and you MUST NOT
store them anywhere. You should instead translate the event into a
format more suitable for your use case or consume it on the spot.

TIMEOUT can be of one of the following:

   T    --- Continuously poll for device events indefinitely.
   NIL  --- Immediately return if no events are pending.
   REAL --- Wait up to the given number of seconds until an event is
            noticed. Returns as soon as an event happened, or some
            time until the timeout runs out.

You should call this function whenever you wish to query the gamepad
for new changes in its buttons or axes.

This function may signal an error of type GAMEPAD-ERROR. When this
happens the device has become unavailable for some reason. You should
remove the device from your own references and invoke the restart
DROP-DEVICE which will remove the device from the internal library and
abort the query.

You must have called INIT prior to calling this function.

See INIT
See DEVICE
See EVENT")
  
  (function rumble
    "Causes haptic feedback on the controller.

STRENGTH should be a number in [0,1] denoting the strength of the
haptic feedback.

PAN should be a number in [-1,+1] denoting where (from left to right)
to rumble.

Note that haptic feedback is wildly inconsistent across devices and
drivers, and may be completely unsupported, or not behave exactly as
described here.

If the haptic feedback is unsupported, this function returns
:UNSUPPORTED.

The rumbling will last at least 0.1 seconds, and may go on
indefinitely. You must modulate the rumbling manually, or stop it
completely by setting the strength to zero.

You must have called INIT prior to calling this function.

See INIT
See DEVICE"))

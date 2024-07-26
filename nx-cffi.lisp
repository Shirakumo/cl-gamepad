(in-package #:org.shirakumo.fraf.gamepad.impl)

(cffi:define-foreign-library nxgamepad
    (T (:or "libnxgamepad.nro" "libnxgamepad")))

(defconstant MAX-DEVICES 9)
(defconstant BUTTON-COUNT 23)
(defconstant AXIS-COUNT 4)

(defvar *NX-BUTTON-LABELS*
  #(;; We already flip AB and XY here.
    :B
    :A
    :Y
    :X
    :L
    :R
    :L1
    :R1
    :L2
    :R2
    :SELECT
    :START
    :DPAD-L
    :DPAD-U
    :DPAD-R
    :DPAD-D
    :L-L
    :L-U
    :L-R
    :L-D
    :R-L
    :R-U
    :R-R
    :R-D))

(defvar *NX-AXIS-LABELS*
  #(:L-H
    :L-V
    :R-H
    :R-V))

(cffi:defcenum device-style
  :none
  :handheld
  :joy-dual
  :joy-left
  :joy-right
  :pro-controller)

(cffi:defcstruct (device :conc-name device-)
  (id :int)
  (style device-style)
  (analog :float :count 4)
  (buttons :uint32)
  (player :int)
  (name :string))

(cffi:defcfun (direct-init "nxgamepad_init") :void
  (multiplayer :int))

(cffi:defcfun (direct-shutdown "nxgamepad_shutdown") :void)

(cffi:defcfun (direct-refresh-devices "nxgamepad_refresh") :bool)

(cffi:defcfun (direct-poll-device "nxgamepad_poll") :bool
  (device-index :int))

(cffi:defcfun (device-count "nxgamepad_count") :int)

(cffi:defcfun (direct-list "nxgamepad_list") :int
  (device-list :pointer)
  (size :int))

(cffi:defcfun (device-index "nxgamepad_index") :int
  (device-id :int))

(cffi:defcfun (device-rumble "nxgamepad_rumble") :int
  (device-index :int)
  (low :float)
  (high :float))

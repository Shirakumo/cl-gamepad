(in-package #:org.shirakumo.fraf.gamepad.test)

(defun effect-capabilities-from-octets (input-octets)
  ;; INPUT-OCTETS here is a list of eight octets representing the
  ;; latter half of device FFBITS in little-endian order (the first
  ;; half is always zeros).  On big-endian systems, we need to reverse
  ;; the octets in each LONG, and the actual layout depends on the
  ;; size of LONG, which differs between 32-bit and 64-bit systems
  ;; (which saves us having to have three copies of all of the test
  ;; vectors, differing only in endinaness and the size of LONG).  And
  ;; just to make sure that we're not going to end up with the same
  ;; code as in PARSE-EFFECT-CAPABILITIES, we're doing this in terms
  ;; of octets instead of in terms of LONG itself.
  (cffi:with-foreign-objects ((ffbits :uint8 16))
    (loop for index from 0 to 7
          do (setf (cffi:mem-aref ffbits :uint8 index) 0))
    (loop with long-mask = #+little-endian 0
                           #+big-endian (1- (cffi:foreign-type-size
                                             :unsigned-long))
          for index from 8
          for octet in input-octets
          do (setf (cffi:mem-aref ffbits :uint8 (logxor index long-mask))
                   octet))
    (org.shirakumo.fraf.gamepad.impl::parse-effect-capabilities ffbits)))

(defun same-set (a b)
  (not (set-exclusive-or a b)))

(define-test parsing-ffbits
  :parent cl-gamepad
  (is same-set nil
      (effect-capabilities-from-octets
       '(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))
  (is same-set '(:rumble)
      (effect-capabilities-from-octets
       '(#x00 #x00 #x01 #x00 #x00 #x00 #x00 #x00)))
  (is same-set '(:periodic)
      (effect-capabilities-from-octets
       '(#x00 #x00 #x02 #x00 #x00 #x00 #x00 #x00)))
  (is same-set '(:rumble :periodic)
      (effect-capabilities-from-octets
       '(#x00 #x00 #x03 #x00 #x00 #x00 #x00 #x00)))
  (is same-set '(:constant)
      (effect-capabilities-from-octets
       '(#x00 #x00 #x04 #x00 #x00 #x00 #x00 #x00)))
  (is same-set '(:spring)
      (effect-capabilities-from-octets
       '(#x00 #x00 #x08 #x00 #x00 #x00 #x00 #x00)))
  (is same-set '(:friction)
      (effect-capabilities-from-octets
       '(#x00 #x00 #x10 #x00 #x00 #x00 #x00 #x00)))
  (is same-set '(:damper)
      (effect-capabilities-from-octets
       '(#x00 #x00 #x20 #x00 #x00 #x00 #x00 #x00)))
  (is same-set '(:inertia)
      (effect-capabilities-from-octets
       '(#x00 #x00 #x40 #x00 #x00 #x00 #x00 #x00)))
  (is same-set '(:ramp)
      (effect-capabilities-from-octets
       '(#x00 #x00 #x80 #x00 #x00 #x00 #x00 #x00)))
  (is same-set '(:periodic :square)
      (effect-capabilities-from-octets
       '(#x00 #x00 #x02 #x01 #x00 #x00 #x00 #x00)))
  (is same-set '(:periodic :triangle)
      (effect-capabilities-from-octets
       '(#x00 #x00 #x02 #x02 #x00 #x00 #x00 #x00)))
  (is same-set '(:periodic :sine)
      (effect-capabilities-from-octets
       '(#x00 #x00 #x02 #x04 #x00 #x00 #x00 #x00)))
  (is same-set '(:periodic :saw-up)
      (effect-capabilities-from-octets
       '(#x00 #x00 #x02 #x08 #x00 #x00 #x00 #x00)))
  (is same-set '(:periodic :saw-down)
      (effect-capabilities-from-octets
       '(#x00 #x00 #x02 #x10 #x00 #x00 #x00 #x00)))
  (is same-set '(:periodic :custom)
      (effect-capabilities-from-octets
       '(#x00 #x00 #x02 #x20 #x00 #x00 #x00 #x00)))
  (is same-set '(:gain)
      (effect-capabilities-from-octets
       '(#x00 #x00 #x00 #x00 #x01 #x00 #x00 #x00)))
  (is same-set '(:autocenter)
      (effect-capabilities-from-octets
       '(#x00 #x00 #x00 #x00 #x02 #x00 #x00 #x00))))

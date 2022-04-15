#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad)

(defun inverse-gethash (value hash-table &optional default)
  (loop for k being the hash-keys of hash-table
        for v being the hash-values of hash-table
        do (when (eq v value) (return k))
        finally (return default)))

(defun out (str &rest args)
  (format *query-io* "~&~?" str args)
  (finish-output *query-io*))

(defun query-labels (labels device map confirm)
  (when (< 0 (length labels))
    (loop with i = 0
          for label = (aref labels i)
          for id = (inverse-gethash label map)
          do (out "Press <~a>~@[ (~a)~]~@[ ~a~] ~60t(<A> to skip, <B> to undo)"
                  label (getf +label-descriptions+ label) id)
             (loop do (flet ((process (event)
                               (when (or (when (typep event 'button-up)
                                           (case (event-label event)
                                             (:A
                                              (out "  Skipped")
                                              (when id (remhash id map))
                                              (incf i))
                                             (:B (decf i))))
                                         (when (funcall confirm event)
                                           (out "  Mapped to ~d" (event-code event))
                                           (incf i)
                                           (setf (gethash (event-code event) map) label)))
                                 (loop-finish))))
                        (poll-events device #'process)))
          while (<= 0 i (1- (length labels))))))

(defun configure-device (device &key (button-labels +common-buttons+)
                                     (axis-labels +common-axes+)
                                     ignored-axes
                                     (mappings-file *default-mappings-file*))
  (let* ((device (ensure-device device))
         (button-map (button-map device))
         (axis-map (axis-map device))
         (orientation-map (orientation-map device))
         (button-copy (copyhash button-map))
         (axis-copy (copyhash axis-map))
         (orientation-copy (copyhash orientation-map)))
    (out "")
    (out "        ┌ ! IMPORTANT, PLEASE READ ! ─────────────────────────────────┐")
    (out "        │ Please follow XBOX controller conventions:                  │")
    (out "        │ Y ─── Ⓨ                                                     │")
    (out "        │ X ─ Ⓧ   Ⓑ ─ B                                               │")
    (out "        │       Ⓐ ─── A                                               │")
    (out "        │ For vertical axes, please only press the axis upwards       │")
    (out "        │ For horizontal axes, please only press the axis right       │")
    (out "        └─────────────────────────────────────────────────────────────┘")
    (out "")
    (out "-> Mapping controller ~s" (name device))
    (clrhash button-map)
    (clrhash axis-map)
    (clrhash orientation-map)
    ;; Drop events until now.
    (poll-events device 'null)
    (out "Press <A> (~a)" (getf +label-descriptions+ :a))
    (loop do (flet ((process (event)
                      (when (typep event 'button-up)
                        (setf (gethash (event-code event) button-map) :a)
                        (loop-finish))))
               (poll-events device #'process)))
    (out "Press <B> (~a)" (getf +label-descriptions+ :b))
    (loop do (flet ((process (event)
                      (when (typep event 'button-up)
                        (setf (gethash (event-code event) button-map) :b)
                        (loop-finish))))
               (poll-events device #'process)))
    
    (out "-> Determining faulty axes. Please do not touch your controller.")
    ;; Sleep and discard to prevent user fudging
    (sleep 1)
    (poll-events device 'null)
    ;; Now watch for bad axes and push them onto ignored.
    (flet ((process (event)
             (when (and (typep event 'axis-move)
                        (< 0.7 (abs (event-value event)))
                        (not (member (event-code event) ignored-axes)))
               (out "  Ignoring ~a" (event-code event))
               (push (event-code event) ignored-axes))))
      (dotimes (i 4)
        (poll-events device #'process :timeout 0.25)))
    
    (out "-> Mapping buttons.~%")
    (query-labels (remove :A (remove :B button-labels)) device button-map
                  (lambda (event) (typep event 'button-up)))
    
    (out "-> Mapping axes.~%")
    (let ((states (make-hash-table :test 'eql)))
      (query-labels axis-labels device axis-map
                    (lambda (event)
                      (when (and (typep event 'axis-move)
                                 (not (find (event-code event) ignored-axes)))
                        (cond ((< (abs (event-value event)) 0.3)
                               (prog1 (eql :high (gethash (event-code event) states))
                                 (setf (gethash (event-code event) states) :low)))
                              ((and (< 0.7 (abs (event-value event)))
                                    (eql :low (gethash (event-code event) states)))
                               (setf (gethash (event-code event) states) :high)
                               (unless (gethash (event-code event) orientation-map)
                                 (setf (gethash (event-code event) orientation-map) (signum (event-value event))))
                               NIL))))))
    
    (out "-> What type of labels are found on the gamepad?~@[ [~a]~]"
         (when (icon-type device) (getf +label-descriptions+ (icon-type device))))
    (loop for icon-type across +icon-types+
          for i from 1
          do (out "~2d) ~a~%" i (getf +label-descriptions+ icon-type)))
    (loop (let ((int (parse-integer (read-line *query-io*) :junk-allowed T)))
            (when int
              (if (<= 0 int (1- (length +icon-types+)))
                  (return (setf (icon-type device) (aref +icon-types+ int)))
                  (out "-> ~d is not a valid type. Try again." int)))))

    (out "-> What should the human-readable name be?~@[ [~a]~]" (name device))
    (let ((name (string-trim '(#\Return #\Linefeed #\Space #\Tab) (read-line *query-io*))))
      (when (string/= "" name)
        (setf (slot-value device 'name) name)))
    
    (cond (mappings-file
           (out "-> Complete. Save configuration? (<A> to confirm, <B> to revert)")
           (loop do (flet ((process (event)
                             (when (typep event 'button-up)
                               (case (event-label event)
                                 (:A
                                  (setf (device-mapping device) device)
                                  (save-device-mappings mappings-file)
                                  (out "-> Saved.")
                                  (loop-finish))
                                 (:B
                                  (copyhash button-copy button-map)
                                  (copyhash axis-copy axis-map)
                                  (copyhash orientation-copy orientation-map)
                                  (out "-> Reverted.")
                                  (loop-finish))))))
                      (poll-events device #'process))))
          (T
           (setf (device-mapping device) device)))
    device))

(defun note-event (ev)
  (typecase ev
    (button-up
     (format T "~& Button ~4a ~6a" (event-code ev) (event-label ev)))
    (axis-move
     (when (<= 0.8 (abs (event-value ev)))
       (format T "~& Axis   ~4a ~6a ~f" (event-code ev) (event-label ev) (event-value ev))))))

(defun monitor-device (device)
  (let ((device (ensure-device device)))
    (out "-> Monitoring controller ~s" (name device))
    (loop do (flet ((process (event)
                      (note-event event)))
               (poll-events device #'process)))))

(defun configurator-main ()
  (with-simple-restart (quit "Quit.")
    (handler-bind (#+sbcl
                   (sb-sys:interactive-interrupt (lambda (e)
                                                   (declare (ignore e))
                                                   (invoke-restart 'quit))))
      (handler-bind ((gamepad-error (lambda (e)
                                      (declare (ignore e))
                                      (invoke-restart 'drop-device))))
        (init))
      (ignore-errors
       (load "device-maps.lisp"))
      (unwind-protect
           (progn
             (loop
                (out "-> Detected the following controllers:")
                (poll-devices)
                (loop for device in (list-devices)
                      for i from 1
                      do (out "~d) ~a" i (name device)))
                (out "-> Please enter the number of a controller to configure, or nothing to exit.~%")
                (let ((input (ignore-errors (parse-integer (read-line *query-io*)))))
                  (unless input
                    (return))
                  (out "-> Enter 1 to configure the device, 2 to monitor the device, or nothing to exit.~%")
                  (case (ignore-errors (parse-integer (read-line *query-io*)))
                    (1 (configure-device (nth (1- input) (list-devices)) :mappings-file NIL))
                    (2 (monitor-device (nth (1- input) (list-devices))))
                    (T (return)))))
             (out "-> Saving device mappings to ~s~%" "device-maps.lisp")
             (save-device-mappings "device-maps.lisp"))
        (shutdown)))))

(in-package #:org.shirakumo.fraf.gamepad)

(defun inverse-gethash (value hash-table &optional default)
  (loop for k being the hash-keys of hash-table
        for v being the hash-values of hash-table
        do (when (eq v value) (return k))
        finally (return default)))

(defun out (str &rest args)
  (format *query-io* "~&~?" str args)
  (finish-output *query-io*))

(defun query-button-press (device confirm)
  (loop with tentative = NIL
        do (flet ((process (event)
                    (typecase event
                      (button-down
                       (setf tentative (event-code event)))
                      (button-up
                       (when (and (eql tentative (event-code event))
                                  (funcall confirm event))
                         (loop-finish))))))
             (poll-events device #'process))))

(defun query-labels (labels device map &optional (confirm (constantly T)))
  (when (< 0 (length labels))
    (loop with i = 0
          for label = (aref labels i)
          for id = (inverse-gethash label map)
          do (out "Press <~a>~@[ (~a)~]~@[ ~a~] ~60t(<A> to skip, <B> to undo)"
                  label (getf +label-descriptions+ label) id)
             (flet ((process (event)
                      (when (or (case (event-label event)
                                  (:A
                                   (out "  Skipped")
                                   (when id (remhash id map))
                                   (incf i))
                                  (:B (decf i)))
                                (when (funcall confirm event)
                                  (out "  Mapped to ~d" (event-code event))
                                  (incf i)
                                  (setf (gethash (event-code event) map) label)))
                        (loop-finish))))
               (query-button-press device #'process))
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
    (flet ((process (event)
             (setf (gethash (event-code event) button-map) :a)))
      (query-button-press device #'process))
    (out "Press <B> (~a)" (getf +label-descriptions+ :b))
    (flet ((process (event)
             (setf (gethash (event-code event) button-map) :b)))
      (query-button-press device #'process))
    
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
    (query-labels (remove :A (remove :B button-labels)) device button-map)
    
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
    (loop (let ((int (1- (parse-integer (read-line *query-io*) :junk-allowed T))))
            (when int
              (if (<= 0 int (1- (length +icon-types+)))
                  (return (setf (icon-type device) (aref +icon-types+ int)))
                  (out "-> ~d is not a valid type. Try again." int)))))

    (out "-> What should the human-readable name be?~@[ [~a]~]~%" (name device))
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
                                  (out "-> Saved to ~a" (namestring mappings-file))
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
     (format T "~& Button ~4a ~6a~%" (event-code ev) (event-label ev)))
    (axis-move
     (when (<= 0.8 (abs (event-value ev)))
       (format T "~& Axis   ~4a ~6a ~f~%" (event-code ev) (event-label ev) (event-value ev))))))

(defun monitor-device (device)
  (let ((device (ensure-device device)))
    (out "-> Monitoring controller ~s" (name device))
    (loop do (flet ((process (event)
                      (note-event event)))
               (poll-events device #'process)))))

(defun %query-for-device ()
  (loop
    (poll-devices)
    (unless (list-devices)
      (out "-> No controllers detected. Polling...")
      (loop until (list-devices)
            do (poll-devices)
               (sleep 0.1)))
    (out "-> Detected the following controllers:")
    (loop for device in (list-devices)
          for i from 1
          do (out "~d) ~a" i (name device)))
    (out "-> Please enter the number of a controller to configure, or q to exit.~%")
    (let* ((input (read-line *query-io*))
           (num (ignore-errors (parse-integer input))))
      (cond ((string-equal "q" input)
             (return))
            ((string-equal "" input)
             (return (first (list-devices))))
            ((null num)
             (out "-> Please enter a proper command."))
            ((not (< 0 num (1+ (length (list-devices)))))
             (out "-> Please enter a proper controller ID."))
            (T
             (return (nth (1- num) (list-devices))))))))

(defun %query-device-action ()
  (loop
    (out "-> Enter 1 to configure the device, 2 to monitor the device, or q to go back.~%")
    (let* ((input (read-line *query-io*))
           (num (ignore-errors (parse-integer input))))
      (cond ((string-equal "q" input)
             (return))
            ((string-equal "" input)
             (return 1))
            ((or (null num) (not (<= 1 num 2)))
             (out "-> Please enter a proper command."))
            (T
             (return num))))))

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
               (let ((dev (%query-for-device)))
                 (unless dev (return))
                 (case (%query-device-action)
                   (1 (configure-device dev :mappings-file NIL))
                   (2 (monitor-device dev))
                   (T (return)))))
             (out "-> Saving device mappings to ~s~%" "device-maps.lisp")
             (save-device-mappings "device-maps.lisp"))
        (shutdown)))))

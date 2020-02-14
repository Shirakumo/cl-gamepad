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

(defun configure-device (device)
  (let ((button-map (button-map device))
        (axis-map (axis-map device)))
    ;; Drop events until now.
    (poll-events device 'null)
    (format *query-io* "~& Mapping buttons.~%")
    (loop for label in +labels+
          for id = (inverse-gethash label button-map)
          do (format *query-io* "~&Press <~a> or enter to leave at ~a" label id)
             (loop do (flet ((process (event)
                               (when (typep event 'button-up)
                                 (setf (gethash (event-code event) button-map) label)
                                 (format *query-io* "~% Mapped to ~a~%" (event-code event))
                                 (loop-finish))))
                        (poll-events device #'process))
                   until (read-char-no-hang *query-io* NIL)))
    (format *query-io* "~& Mapping axes.~%")
    (loop for label in +labels+
          for id = (inverse-gethash label axis-map)
          do (format *query-io* "~&Hold <~a> or enter to leave at ~a" label id)
             (loop do (flet ((process (event)
                               (when (and (typep event 'axis-move)
                                          (< 0.8 (event-value event)))
                                 (setf (gethash (event-code event) button-map) label)
                                 (format *query-io* "~% Mapped to ~a~%" (event-code event))
                                 (loop-finish))))
                        (poll-events device #'process))
                   until (read-char-no-hang *query-io* NIL)))
    (format *query-io* "~&~% Complete. Save configuration? [Y/n] ")
    (let ((line (read-line *query-io*)))
      (unless (or (string-equal line "n") (string-equal line "no"))
        (setf (device-mapping device) device)
        (save-device-mappings)))))

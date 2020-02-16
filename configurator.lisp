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
  (loop with i = 0
        for label = (aref labels i)
        for id = (inverse-gethash label map)
        do (out "Press <~a>~@[ ~a~] ~30t(<A> to skip, <B> to undo)" label id)
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
        while (<= 0 i (1- (length labels)))))

(defun configure-device (device &key (button-labels +common-buttons+)
                                     (axis-labels +common-axes+)
                                     ignored-axes)
  (let* ((button-map (button-map device))
         (axis-map (axis-map device))
         (button-copy (copyhash button-map))
         (axis-copy (copyhash axis-map)))
    ;; Drop events until now.
    (out "-> Mapping ~s" (name device))
    (clrhash button-map)
    (clrhash axis-map)
    (poll-events device 'null)
    (out "Press <A>")
    (loop do (flet ((process (event)
                      (when (typep event 'button-up)
                        (setf (gethash (event-code event) button-map) :a)
                        (loop-finish))))
               (poll-events device #'process)))
    (out "Press <B>")
    (loop do (flet ((process (event)
                      (when (typep event 'button-up)
                        (setf (gethash (event-code event) button-map) :b)
                        (loop-finish))))
               (poll-events device #'process)))
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
                              ((and (< 0.8 (abs (event-value event)))
                                    (eql :low (gethash (event-code event) states)))
                               (setf (gethash (event-code event) states) :high)
                               NIL))))))
    (out "-> Complete. Save configuration? (<A> to confirm, <B> to revert)")
    (loop do (flet ((process (event)
                      (when (typep event 'button-up)
                        (case (event-label event)
                          (:A
                           (setf (device-mapping device) device)
                           (save-device-mappings)
                           (out "-> Saved.")
                           (loop-finish))
                          (:B
                           (copyhash button-copy button-map)
                           (copyhash axis-copy axis-map)
                           (out "-> Reverted.")
                           (loop-finish))))))
               (poll-events device #'process)))
    device))

(defun note-event (ev)
  (typecase ev
    (button-up
     (format T "~& Button ~4x ~6a" (event-code ev) (event-label ev)))
    (axis-move
     (when (<= 0.8 (abs (event-value ev)))
       (format T "~& Axis   ~4x ~6a ~f" (event-code ev) (event-label ev) (event-value ev))))))

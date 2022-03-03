#|
 This file is a part of cl-gamepad
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gamepad)

(defvar *here* #.(make-pathname :name NIL :type NIL :defaults (or *compile-file-pathname* *load-pathname*
                                                                  (error "You must COMPILE-FILE or LOAD this file."))))
(defvar *default-mappings-file* (make-pathname :name "default-device-mappings" :type "lisp" :defaults *here*))
(defvar *device-mappings* (make-hash-table :test 'equalp))

(defun normalize-mapping-id (id)
  (etypecase id
    (device
     (list (driver id) (vendor id) (product id)))
    (cons
     (check-type id (cons keyword (cons integer (cons integer null))))
     id)))

(defun mapping-id< (a b)
  (destructuring-bind (ad av ap) a
    (destructuring-bind (bd bv bp) b
      (or (string< ad bd)
          (and (string= ad bd)
               (or (< av bv)
                   (and (= av bv)
                        (< ap bp))))))))

(defun copyhash (from &optional (to (make-hash-table :test (hash-table-test from))))
  (unless (eq from to)
    (clrhash to)
    (maphash (lambda (k v) (setf (gethash k to) v)) from))
  to)

(defun update-mapping-in-device (device mapping)
  (when (getf mapping :name)
    (setf (slot-value device 'name) (getf mapping :name)))
  (when (getf mapping :icon-type)
    (setf (icon-type device) (getf mapping :icon-type)))
  (setf (button-map device) (or (getf mapping :buttons)
                                (error "Malformed mapping, missing :BUTTONS")))
  (setf (axis-map device) (or (getf mapping :axes)
                              (error "Malformed mapping, missing :AXES")))
  (setf (orientation-map device) (or (getf mapping :orientations)
                                     (make-hash-table :test 'eql))))

(defmethod initialize-instance :after ((device device) &key)
  (let ((mapping (device-mapping device)))
    (when mapping (update-mapping-in-device device mapping))))

(defun device-mapping (id)
  (gethash (normalize-mapping-id id) *device-mappings*))

(defun (setf device-mapping) (mapping id)
  (let* ((id (normalize-mapping-id id))
         (mapping (etypecase mapping
                    (cons mapping)
                    (device (list :name (name mapping)
                                  :buttons (button-map mapping)
                                  :axes (axis-map mapping)
                                  :orientations (orientation-map mapping)
                                  :icon-type (icon-type mapping)))))
         (known (device-mapping id)))
    (cond (known
           ;; Update the values in place to immediately update all
           ;; devices using it, too. Don't update the name unless
           ;; necessary as the device default is probably less accurate
           (unless (getf known :name)
             (setf (getf known :name) (getf mapping :name)))
           (when (getf mapping :icon-type)
             (setf (getf known :icon-type) (getf mapping :icon-type)))
           (copyhash (getf mapping :axes) (getf known :axes))
           (copyhash (getf mapping :buttons) (getf known :buttons))
           (copyhash (getf mapping :orientations) (getf known :orientations)))
          (T
           (setf (gethash id *device-mappings*) mapping)
           ;; Need to go through all devices to see if they match
           ;; the new mapping.
           (dolist (device (list-devices))
             (when (equalp id (normalize-mapping-id device))
               (update-mapping-in-device device mapping)))))))

(defun remove-device-mapping (id)
  (remhash (normalize-mapping-id id) *device-mappings*))

(defun map-plist (table)
  (flet ((label-pos (thing)
           (if (symbolp (second thing))
               (position (second thing) +labels+)
               (first thing))))
    (loop for (k v) in (sort (loop for k being the hash-keys of table
                                   for v being the hash-values of table
                                   collect (list k v))
                             #'< :key #'label-pos)
          collect k collect v)))

(defun plist-map (plist)
  (let ((map (make-hash-table :test 'eql)))
    (loop for (k v) on plist by #'cddr
          do (setf (gethash k map) v))
    map))

(defmacro define-device-mapping ((driver vendor product) &body plist)
  `(setf (device-mapping '(,driver ,vendor ,product))
         (list :name ,(getf plist :name)
               :buttons (plist-map ',(getf plist :buttons))
               :axes (plist-map ',(getf plist :axes))
               :orientations (plist-map ',(getf plist :orientations))
               :icon-type ,(getf plist :icon-type :generic-xbox))))

(defun save-device-mappings (&optional (file *default-mappings-file*))
  (with-open-file (stream file :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create
                               :external-format :utf-8)
    (format stream ";;;; Device mapping definitions
;;; This file is auto-generated. Do not edit it manually
;;; unless you know what you are doing. You can generate
;;; the file from current mappings in your Lisp image via
;;; 
;;;   (org.shirakumo.fraf.gamepad:save-device-mappings)
;;; 
;;; You can also interactively define new mappings using
;;; 
;;;   (org.shirakumo.fraf.gamepad:configure-device device)
;;;
\(in-package ~s)~%" (package-name #.*package*))
    (loop for (id . map) in (sort (loop for k being the hash-keys of *device-mappings*
                                        for v being the hash-values of *device-mappings*
                                        collect (cons k v))
                                  #'mapping-id< :key #'car)
          do (format stream "~%(define-device-mapping ~s" id)
             (format stream "~%  :name ~s" (getf map :name))
             (format stream "~%  :icon-type ~s" (getf map :icon-type :generic-xbox))
             (format stream "~%  :buttons ~s" (map-plist (getf map :buttons)))
             (format stream "~%  :axes ~s" (map-plist (getf map :axes)))
             (format stream "~%  :orientations ~s)~%" (map-plist (getf map :orientations))))))

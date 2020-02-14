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

(defun device-mapping (id)
  (gethash (normalize-mapping-id id) *device-mappings*))

(defun (setf device-mapping) (mapping id)
  (setf (gethash (normalize-mapping-id id) *device-mappings*) mapping))

(defun remove-device-mapping (id)
  (remhash (normalize-mapping-id id) *device-mappings*))

(defun map-plist (table)
  (loop for k being the hash-keys of table
        for v being the hash-values of table
        collect k collect v))

(defun plist-map (plist)
  (let ((map (make-hash-table :test 'eql)))
    (loop for (k v) on plist by #'cddr
          do (setf (gethash k map) v))
    map))

(defmacro define-device-mapping ((driver vendor product) &body plist)
  `(setf (device-mapping '(,driver ,vendor ,product))
         (list :name ,(getf plist :name)
               :buttons (plist-map ',(getf plist :buttons))
               :axes (plist-map ',(getf plist :axes)))))

(defun save-device-mappings (&optional (file *default-mappings-file*))
  (with-open-file (stream file :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create
                               :external-format :utf-8)
    (format stream "~
;;;; Device mapping definitions
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
                                  #'mapping-id<)
          do (format stream "~%(define-device-mapping ~s" id)
             (format stream "~%  :name ~s" (getf map :name))
             (format stream "~%  :buttons ~s" (map-plist (getf map :buttons)))
             (format stream "~%  :axes ~s)~%" (map-plist (getf map :axes))))))

(asdf:load-system :staple-markless)

(defpackage "gamepad-docs"
  (:use #:cl)
  (:local-nicknames
   (#:gamepad #:org.shirakumo.fraf.gamepad)))

(defclass page* (staple:simple-page)
  ()
  (:default-initargs :document-package (find-package "gamepad-docs")))

(defmethod staple:subsystems ((system (eql (asdf:find-system :cl-gamepad))))
  ())

(defmethod staple:page-type ((system (eql (asdf:find-system :cl-gamepad))))
  'page*)

(defmethod staple:packages ((system (eql (asdf:find-system :cl-gamepad))))
  (list (find-package (string '#:org.shirakumo.fraf.gamepad))))

#+sbcl
(defmethod staple:definition-wanted-p ((definition definitions:source-transform) (page page*)) NIL)


(in-package :cl-user)
(defpackage mini-sdl2-asd
  (:use :cl :asdf))
(in-package :mini-sdl2-asd)

(asdf:defsystem :mini-sdl2
  :version "1.0"
  :author "BIS"
  :license "zlib license"
  :depends-on (:cffi)
  :components ((:file "package")
               (:file "wrapper" :depends-on ("package"))
               (:module "src"
                :depends-on ("wrapper")
                :components ((:file "sdl2")
                             (:file "video")
                             (:file "audio")
                             (:file "input")
                             (:file "event"))))
  :description "Binding for a small part of SDL2.")



(in-package :cl-user)
(defpackage mini-sdl2-asd
  (:use :cl :asdf))
(in-package :mini-sdl2-asd)

(asdf:defsystem :mini-sdl2
  :version "0.9"
  :author "BIS"
  :license "zlib license"
  :depends-on (:cffi)
  :components ((:file "package")
               (:file "wrapper" :depends-on ("package"))
               (:module "src"
                :depends-on ("wrapper")
                :components ((:file "sdl2")
                             (:file "video" :depends-on ("sdl2"))
                             (:file "audio" :depends-on ("sdl2"))
                             (:file "input" :depends-on ("sdl2"))
                             (:file "event" :depends-on ("sdl2"))
                             (:file "gltex" :depends-on ("video")))))
  :description "Binding for a small part of SDL2.")


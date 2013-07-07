
(in-package :cl-user)
(defpackage :mini-sdl2
  (:nicknames :sdl2)
  (:use :cl :cffi))
(in-package :mini-sdl2)

(define-foreign-library sdl2
  (:windows (:default "SDL2"))
  (t (:default "libSDL2")))

(define-foreign-library sdl2-image
  (:windows (:default "SDL2_image"))
  (t (:default "libSDL2_image")))

(define-foreign-library sdl2-mixer
  (:windows (:default "SDL2_mixer"))
  (t (:default "libSDL2_mixer")))

(define-foreign-library sdl2-ttf
  (:windows (:default "SDL2_ttf"))
  (t (:default "libSDL2_ttf")))

(use-foreign-library sdl2)
(use-foreign-library sdl2-image)
(use-foreign-library sdl2-mixer)
(use-foreign-library sdl2-ttf)


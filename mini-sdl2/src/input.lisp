
(in-package :mini-sdl2)

(defcfun ("SDL_NumJoysticks" %num-joysticks) :int)
(defcfun ("SDL_JoystickOpen" %joystick-open) :pointer (device-index :int))
(defcfun ("SDL_JoystickOpened" %joystick-opened) :int (device-index :int))
(defcfun ("SDL_JoystickClose" %joystick-close) :void (joystick :pointer))
(defcfun ("SDL_JoystickInstanceID" %joystick-index) :int (joystick :pointer))
(defcfun ("SDL_JoystickName" %joystick-name) :string (joystick :pointer))
(defcfun ("SDL_JoystickNumAxes" %joystick-num-axes) :int (joystick :pointer))
(defcfun ("SDL_JoystickNumBalls" %joystick-num-balls) :int (joystick :pointer))
(defcfun ("SDL_JoystickNumButtons" %joystick-num-buttons) :int (joystick :pointer))
(defcfun ("SDL_JoystickNumHats" %joystick-num-hats) :int (joysitck :pointer))

(defparameter *joystick-list* ())

(defun %get-active-joystick (index)
  (some (lambda (j) (= (%joystick-index j) index)) *joystick-list*))

(defun num-joysticks () (%num-joysticks))

(defun joystick-style (index)
  (let ((joy (%get-active-joystick index)))
    (if joy (list (%joystick-name joy)
                  (%joystick-num-axes)
                  (%joystick-num-balls)
                  (%joysitck-num-buttons)
                  (%joysitck-num-hats)))))

(defun joystick-open (index)
  (unless (%get-active-joystick index)
    (push (%joystick-open index) *joystick-list*)))

(defun joystick-opened (index)
  (%get-active-joystick index))

(defun joystick-close (index)
  (let ((joy (%get-active-joystick index)))
    (when joy (%joystick-close joy)
              (remove joy *joystick-list*))))

(export
  '(num-joysticks
    joystick-style
    joystick-open
    joystick-opened
    joystick-close))



(in-package :mini-sdl2)

(defcfun ("SDL_PollEvent" %poll-event) :int
  (event :pointer))
(defcfun ("SDL_PumpEvents" %pump-events) :void)
(defcfun ("SDL_PushEvent" %push-event) :int
  (event :pointer))

;;; Quit-Request

(defun quit-request ()
  (with-foreign-object (event '(:union event))
    (setf (foreign-slot-value event '(:union event) :type)
          (foreign-enum-value 'event-type :quit))
    (check-error (%push-event event))))

;;; Event-Loop

(defmacro loop-event-handling
  (&optional (recv (gensym))
   &key window keyboard text-editing text-input
        mouse-motion mouse-button mouse-wheel
        joy-axis joy-ball joy-hat joy-button joy-device
        quit idle)
  `(let ((*leave-event-loop-flag* nil))
    (with-foreign-object (,recv '(:union event))
      (loop until *leave-event-loop-flag* do
        (if (> (%poll-event ,recv) 0)
          (case (%event-value ,recv :type)
            ((,(%event-type-value :window)) ,window)
            ((,(%event-type-value :quit)) ,quit)
            ((,(%event-type-value :keydown)
              ,(%event-type-value :keyup)) ,keyboard)
            ((,(%event-type-value :text-editing)) ,text-editing)
            ((,(%event-type-value :text-input)) ,text-input)
            ((,(%event-type-value :mouse-motion)) ,mouse-motion)
            ((,(%event-type-value :mouse-button-down)
              ,(%event-type-value :mouse-button-up)) ,mouse-button)
            ((,(%event-type-value :mouse-wheel)) ,mouse-wheel)
            ((,(%event-type-value :joy-axis-motion)) ,joy-axis)
            ((,(%event-type-value :joy-ball-motion)) ,joy-ball)
            ((,(%event-type-value :joy-hat-motion)) ,joy-hat)
            ((,(%event-type-value :joy-button-down)
              ,(%event-type-value :joy-button-up)) ,joy-button)
            (otherwise nil))
          ,idle)))))

;;; Event-Loop Return
(defvar *leave-event-loop-flag*)
(defun leave-event-loop () (setf *leave-event-loop-flag* t))

;;; With-Event

(defmacro with-event-slot (event-type spec &body body)
  (case event-type
    ((:window)
      (destructuring-bind (handle &key event timestamp window-id win-event data1 data2) spec
        (%make-let body
                   event `(%event-type-symbol (%window-event-value ,handle :type))
                   timestamp `(%window-event-value ,handle :timestamp)
                   window-id `(%window-event-value ,handle :window-id)
                   win-event `(%window-event-type-symbol (%window-event-value ,handle :event))
                   data1 `(%window-event-value ,handle :data1)
                   data2 `(%window-event-value ,handle :data2))))
    ((:keyboard)
      (destructuring-bind (handle &key event timestamp window-id state repeat keysym) spec
        (%make-let body
                   event `(%event-type-symbol (%keyboard-event-value ,handle :type))
                   timestamp `(%keyboard-event-value ,handle :timestamp)
                   window-id `(%keyboard-event-value ,handle :window-id)
                   state `(%keyboard-event-value ,handle :state)
                   repeat `(%keyboard-event-value ,handle :repeat)
                   keysym `(%keysym-value (%keyboard-event-pointer ,handle :keysym) :keycode))))
    ((:text-editing)
      (destructuring-bind (handle &key event timestamp window-id text start length) spec
        (%make-let body
                   event `(%event-type-symbol (%text-editing-event-value ,handle :type))
                   timestamp `(%text-editing-event-value ,handle :timestamp)
                   window-id `(%text-editing-event-value ,handle :window-id)
                   text `(foreign-string-to-lisp (%text-editing-event-value ,handle :text))
                   start `(%text-editing-event-value ,handle :start)
                   length `(%text-editing-event-value ,handle :length))))
    ((:text-input)
      (destructuring-bind (handle &key event timestamp window-id text) spec
        (%make-let body
                   event `(%event-type-symbol (%text-input-event-value ,handle :type))
                   timestamp `(%text-input-event-value ,handle :timestamp)
                   window-id `(%text-input-event-value ,handle :window-id)
                   text `(foreign-string-to-lisp (%text-input-event-value ,handle :text)))))
    ((:mouse-motion)
      (destructuring-bind (handle &key event timestamp window-id state x y xrel yrel) spec
        (%make-let body
                   event `(%event-type-symbol (%mouse-motion-event-value ,handle :type))
                   timestamp `(%mouse-motion-event-value ,handle :timestamp)
                   window-id `(%mouse-motion-event-value ,handle :window-id)
                   state `(%mouse-button-flags-symbols (%mouse-motion-event-value ,handle :state))
                   x `(%mouse-motion-event-value ,handle :x)
                   y `(%mouse-motion-event-value ,handle :y)
                   xrel `(%mouse-motion-event-value ,handle :xrel)
                   yrel `(%mouse-motion-event-value ,handle :yrel))))
    ((:mouse-button)
      (destructuring-bind (handle &key event timestamp window-id button state x y) spec
        (%make-let body
                   event `(%event-type-symbol (%mouse-button-event-value ,handle :type))
                   timestamp `(%mouse-button-event-value ,handle :timestamp)
                   window-id `(%mouse-button-event-value ,handle :window-id)
                   button `(%mouse-button-type-symbol (%mouse-button-event-value ,handle :button))
                   state `(%button-state-symbol (%mouse-button-event-value ,handle :state))
                   x `(%mouse-button-event-value ,handle :x)
                   y `(%mouse-button-event-value ,handle :y))))
    ((:mouse-wheel)
      (destructuring-bind (handle &key event timestamp window-id x y) spec
        (%make-let body
                   event `(%event-type-symbol (%mouse-wheel-event-value ,handle :type))
                   timestamp `(%mouse-wheel-event-value ,handle :timestamp)
                   window-id `(%mouse-wheel-event-value ,handle :window-id)
                   x `(%mouse-wheel-event-value ,handle :x)
                   y `(%mouse-wheel-event-value ,handle :y))))
    ((:joy-axis)
      (destructuring-bind (handle &key event timestamp which axis value) spec
        (%make-let body
                   event `(%event-type-symbol (%joy-axis-event-value ,handle :type))
                   timestamp `(%joy-axis-event-value ,handle :timestamp)
                   which `(%joy-axis-event-value ,handle :which)
                   axis `(%joy-axis-event-value ,handle :axis)
                   value `(%joy-axis-event-value ,handle :value))))
    ((:joy-ball)
      (destructuring-bind (handle &key event timestamp which ball xrel yrel) spec
        (%make-let body
                   event `(%event-type-symbol (%joy-ball-event-value ,handle :type))
                   timestamp `(%joy-ball-event-value ,handle :timestamp)
                   which `(%joy-ball-event-value ,handle :which)
                   ball `(%joy-ball-event-value ,handle :ball)
                   xrel `(%joy-ball-event-value ,handle :xrel)
                   yrel `(%joy-ball-event-value ,handle :yrel))))
    ((:joy-hat)
      (destructuring-bind (handle &key event timestamp which hat value) spec
        (%make-let body
                   event `(%event-type-symbol (%joy-hat-event-value ,handle :type))
                   timestamp `(%joy-hat-event-value ,handle :timestamp)
                   which `(%joy-hat-event-value ,handle :which)
                   hat `(%joy-hat-event-value ,handle :hat)
                   value `(%hat-position-symbol (%joy-hat-event-value ,handle :value)))))
    ((:joy-button)
      (destructuring-bind (handle &key event timestamp which button state) spec
        (%make-let body
                   event `(%event-type-symbol (%joy-button-event-value ,handle :type))
                   timestamp `(%joy-button-event-value ,handle :timestamp)
                   which `(%joy-button-event-value ,handle :which)
                   button `(%joy-button-event-value ,handle :button)
                   state `(%button-state-symbol (%joy-button-event-value ,handle :state)))))
    ((:quit)
      (destructuring-bind (handle &key event timestamp) spec
        (%make-let body
                   event-type `(%event-type-symbol (%quit-event-value ,handle :type))
                   timestamp `(%joy-quit-event-value ,handle :timestamp))))))

(export
  '(quit-request
    loop-event-handling
    leave-event-loop
    with-event-slot))


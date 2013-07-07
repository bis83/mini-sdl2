
(in-package :mini-sdl2)

(defcfun ("SDL_PollEvent" %poll-event) :int (event :pointer))
(defcfun ("SDL_PumpEvents" %pump-events) :void)
(defcfun ("SDL_PushEvent" %push-event) :int (event :pointer))

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

(defun %list-slot (&rest args)
  (loop for sym in args by #'cddr
        for frm in (cdr args) by #'cddr
        when sym collect (list sym frm)))

;;; With-Event

(defmacro with-window-event (spec &body body)
  (destructuring-bind (ev &key event-type timestamp window-id event data1 data2) spec
    `(let ,(%list-slot
            event-type `(%event-type-symbol (%window-event-value ,ev :type))
            timestamp `(%window-event-value ,ev :timestamp)
            window-id `(%window-event-value ,ev :window-id)
            event `(%window-event-type-symbol (%window-event-value ,ev :event))
            data1 `(%window-event-value ,ev :data1)
            data2 `(%window-event-value ,ev :data2))
      ,@body)))

(defmacro with-keyboard-event (spec &body body)
  (destructuring-bind (ev &key event-type timestamp window-id state repeat keysym) spec
    `(let ,(%list-slot
            event-type `(%event-type-symbol (%keyboard-event-value ,ev :type))
            timestamp `(%keyboard-event-value ,ev :timestamp)
            window-id `(%keyboard-event-value ,ev :window-id)
            state `(%keyboard-event-value ,ev :state)
            repeat `(%keyboard-event-value ,ev :repeat)
            keysym `(%keysym-value (%keyboard-event-pointer ,ev :keysym) :keycode))
      ,@body)))

(defmacro with-text-editing-event (spec &body body)
  (destructuring-bind (ev &key event-type timestamp window-id text start length) spec
    `(let ,(%list-slot
            event-type `(%event-type-symbol (%text-editing-event-value ,ev :type))
            timestamp `(%text-editing-event-value ,ev :timestamp)
            window-id `(%text-editing-event-value ,ev :window-id)
            text `(foreign-string-to-lisp (%text-editing-event-value ,ev :text))
            start `(%text-editing-event-value ,ev :start)
            length `(%text-editing-event-value ,ev :length))
      ,@body)))

(defmacro with-text-input-event (spec &body body)
  (destructuring-bind (ev &key event-type timestamp window-id text) spec
    `(let ,(%list-slot
            event-type `(%event-type-symbol (%text-input-event-value ,ev :type))
            timestamp `(%text-input-event-value ,ev :timestamp)
            window-id `(%text-input-event-value ,ev :window-id)
            text `(foreign-string-to-lisp (%text-input-event-value ,ev :text)))
      ,@body)))

(defmacro with-mouse-motion-event (spec &body body)
  (destructuring-bind (ev &key event-type timestamp window-id state x y xrel yrel) spec
    `(let ,(%list-slot
            event-type `(%event-type-symbol (%mouse-motion-event-value ,ev :type))
            timestamp `(%mouse-motion-event-value ,ev :timestamp)
            window-id `(%mouse-motion-event-value ,ev :window-id)
            state `(%mouse-button-flags-symbols (%mouse-motion-event-value ,ev :state))
            x `(%mouse-motion-event-value ,ev :x)
            y `(%mouse-motion-event-value ,ev :y)
            xrel `(%mouse-motion-event-value ,ev :xrel)
            yrel `(%mouse-motion-event-value ,ev :yrel))
      ,@body)))

(defmacro with-mouse-button-event (spec &body body)
  (destructuring-bind (ev &key event-type timestamp window-id button state x y) spec
    `(let ,(%list-slot
            event-type `(%event-type-symbol (%mouse-button-event-value ,ev :type))
            timestamp `(%mouse-button-event-value ,ev :timestamp)
            window-id `(%mouse-button-event-value ,ev :window-id)
            button `(%mouse-button-type-symbol (%mouse-button-event-value ,ev :button))
            state `(%button-state-symbol (%mouse-button-event-value ,ev :state))
            x `(%mouse-button-event-value ,ev :x)
            y `(%mouse-button-event-value ,ev :y))
      ,@body)))

(defmacro with-mouse-wheel-event (spec &body body)
  (destructuring-bind (ev &key event-type timestamp window-id x y) spec
    `(let ,(%list-slot
            event-type `(%event-type-symbol (%mouse-wheel-event-value ,ev :type))
            timestamp `(%mouse-wheel-event-value ,ev :timestamp)
            window-id `(%mouse-wheel-event-value ,ev :window-id)
            x `(%mouse-wheel-event-value ,ev :x)
            y `(%mouse-wheel-event-value ,ev :y))
      ,@body)))

(defmacro with-joy-axis-event (spec &body body)
  (destructuring-bind (ev &key event-type timestamp which axis value) spec
    `(let ,(%list-slot
            event-type `(%event-type-symbol (%joy-axis-event-value ,ev :type))
            timestamp `(%joy-axis-event-value ,ev :timestamp)
            which `(%joy-axis-event-value ,ev :which)
            axis `(%joy-axis-event-value ,ev :axis)
            value `(%joy-axis-event-value ,ev :value))
      ,@body)))

(defmacro with-joy-ball-event (spec &body body)
  (destructuring-bind (ev &key event-type timestamp which ball xrel yrel) spec
    `(let ,(%list-slot
            event-type `(%event-type-symbol (%joy-ball-event-value ,ev :type))
            timestamp `(%joy-ball-event-value ,ev :timestamp)
            which `(%joy-ball-event-value ,ev :which)
            ball `(%joy-ball-event-value ,ev :ball)
            xrel `(%joy-ball-event-value ,ev :xrel)
            yrel `(%joy-ball-event-value ,ev :yrel))
      ,@body)))

(defmacro with-joy-hat-event (spec &body body)
  (destructuring-bind (ev &key event-type timestamp which hat value) spec
    `(let ,(%list-slot
            event-type `(%event-type-symbol (%joy-hat-event-value ,ev :type))
            timestamp `(%joy-hat-event-value ,ev :timestamp)
            which `(%joy-hat-event-value ,ev :which)
            hat `(%joy-hat-event-value ,ev :hat)
            value `(%hat-position-symbol (%joy-hat-event-value ,ev :value)))
      ,@body))) 

(defmacro with-joy-button-event (spec &body body)
  (destructuring-bind (ev &key event-type timestamp which button state) spec
    `(let ,(%list-slot
            event-type `(%event-type-symbol (%joy-button-event-value ,ev :type))
            timestamp `(%joy-button-event-value ,ev :timestamp)
            which `(%joy-button-event-value ,ev :which)
            button `(%joy-button-event-value ,ev :button)
            state `(%button-state-symbol (%joy-button-event-value ,ev :state)))
      ,@body))) 

(defmacro with-quit-event (spec &body body)
  (destructuring-bind (ev &key event-type timestamp) spec
    `(let ,(%list-slot
            event-type `(%event-type-symbol (%quit-event-value ,ev :type))
            timestamp `(%joy-quit-event-value ,ev :timestamp))
      ,@body)))

(export
  '(quit-request
    loop-event-handling
    leave-event-loop
    with-window-event
    with-keyboard-event
    with-text-editing-event
    with-text-input-event
    with-mouse-motion-event
    with-mouse-button-event
    with-mouse-wheel-event
    with-joy-axis-event
    with-joy-ball-event
    with-joy-hat-event
    with-joy-button-event
    with-joy-device-event
    with-joy-quit-event))


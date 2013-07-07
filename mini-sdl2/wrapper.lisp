
(in-package :mini-sdl2)

(eval-when (:load-toplevel :compile-toplevel)
  (defun %list-no-nil (&rest args)
    (remove nil args)))

(defmacro %defenum (name spec &body body)
  (destructuring-bind (&key symbol value) spec
    (%list-no-nil
      'progn
      (if symbol
        (let ((obj (gensym)))
          `(progn
            (declaim (inline ,symbol))
            (defun ,symbol (,obj)
              (foreign-enum-keyword ',name ,obj)))))
      (if value
        (let ((obj (gensym)))
          `(progn
            (declaim (inline ,value))
            (defun ,value (,obj)
              (foreign-enum-value ',name ,obj)))))
      `(defcenum ,name ,@body))))

(defmacro %defstruct (name spec &body body)
  (destructuring-bind (&key value pointer) spec
    (%list-no-nil
      'progn
      (if value
        (let ((obj (gensym)) (key (gensym)))
          `(progn
            (declaim (inline ,value))
            (defun ,value (,obj ,key)
              (foreign-slot-value ,obj '(:struct ,name) ,key)))))
      (if pointer
        (let ((obj (gensym)) (key (gensym)))
          `(progn
            (declaim (inline ,pointer))
            (defun ,pointer (,obj ,key)
              (foreign-slot-pointer ,obj '(:struct ,name) ,key)))))
      `(defcstruct ,name ,@body))))

(defmacro %defunion (name spec &body body)
  (destructuring-bind (&key value pointer) spec
    (%list-no-nil
      'progn
      (if value
        (let ((obj (gensym)) (key (gensym)))
          `(progn
            (declaim (inline ,value))
            (defun ,value (,obj ,key) 
              (foreign-slot-value ,obj '(:union ,name) ,key)))))
      (if pointer
        (let ((obj (gensym)) (key (gensym)))
          `(progn
            (declaim (inline ,pointer))
            (defun ,pointer (,obj ,key)
              (foreign-slot-pointer ,obj '(:union ,name) ,key)))))
      `(defcunion ,name ,@body))))

(defmacro %defbitfield (name spec &body body)
  (destructuring-bind (&key symbols value) spec
    (%list-no-nil
      'progn
      (if symbols
        (let ((val (gensym)))
          `(progn
            (declaim (inline ,symbols))
            (defun ,symbols (,val)
              (foreign-bitfield-symbols ',name ,val)))))
      (if value
        (let ((val (gensym)))
          `(progn
            (declaim (inline ,value))
            (defun ,value (,val)
              (foreign-bitfield-value ',name ,val)))))
      `(defbitfield ,name ,@body))))

;;; Wrapper SDL2 Struct, Enum, and Bitfield

(%defbitfield init-flags (:value %init-flags-value
                          :symbols %init-flags-symbols)
  (:audio    #x00000010)
  (:video    #x00000020)
  (:joystick #x00000200)
  (:haptic   #x00001000))

(%defenum event-state (:value %event-state-value
                       :symbol %event-state-symbol)
  (:query -1)
  (:ignore 0)
  (:disable 0)
  (:enable 1))

(%defenum event-type (:value %event-type-value
                      :symbol %event-type-symbol)
  (:quit #x100)
  (:window #x200)
  :system
  (:keydown #x300)
  :keyup
  :text-editing
  :text-input
  (:mouse-motion #x400)
  :mouse-button-down
  :mouse-button-up
  :mouse-wheel
  (:joy-axis-motion #x600)
  :joy-ball-motion
  :joy-hat-motion
  :joy-button-down
  :joy-button-up)

(%defenum window-event-type (:value %window-event-type-value
                             :symbol %window-event-type-symbol)
  :none
  :shown
  :hidden
  :exposed
  :moved
  :resized
  :size-changed
  :minimized
  :maximized
  :restored
  :enter
  :leave
  :focus-gained
  :focus-lost
  :close)

(%defbitfield window-flags (:value %window-flags-value
                            :symbols %window-flags-symbol)
  (:fullscreen #x00000001)
  (:opengl #x00000002)
  (:shown #x00000004)
  (:hidden #x00000008)
  (:borderless #x00000010)
  (:resizable #x00000020)
  (:minimized #x00000040)
  (:maximized #x00000080)
  (:input-grabbed #x00000100)
  (:input-focus #x00000200)
  (:mouse-focus #x00000400)
  (:fullscreen-desktop #x00001001)
  (:foreign #x00000800))

(%defenum button-state (:value %button-state-value
                        :symbol %button-state-symbol)
  (:released 0)
  (:pressed 1))

;;; Keyboard

(%defstruct keysym (:value %keysym-value
                    :pointer %keysym-pointer)
  (:scancode :uint32)
  (:keycode :int32)
  (:mod :uint16)
  (:unused :uint32))

;;; Mouse

(%defbitfield mouse-button-flags (:value %mouse-button-flags-values
                                  :symbols %mouse-button-flags-symbols)
  :left
  :middle
  :right
  :x1
  :x2)

(%defenum mouse-button-type (:value %mouse-button-type-value
                             :symbol %mouse-button-type-symbol)
  (:left 1)
  (:middle 2)
  (:right 3)
  (:x1 4)
  (:x2 5))

;;; Joystick

(%defenum hat-position (:value %hat-position-value
                        :symbol %hat-position-symbol)
  (:centered #x00)
  (:up #x01)
  (:right #x02)
  (:down #x04)
  (:left #x08)
  (:up-right #x03)
  (:up-left #x09)
  (:down-right #x06)
  (:down-left #xC))

;;; Event

(%defstruct common-event (:value %common-event-value
                          :pointer %common-event-pointer)
  (:type :uint32)
  (:timestamp :uint32))

(%defstruct window-event (:value %window-event-value
                          :pointer %window-event-pointer)
  (:type :uint32)
  (:timestamp :uint32)
  (:window-id :uint32)
  (:event :uint8)
  (:padding1 :uint8)
  (:padding2 :uint8)
  (:padding3 :uint8)
  (:data1 :int32)
  (:data2 :int32))

(%defstruct keyboard-event (:value %keyboard-event-value
                            :pointer %keyboard-event-pointer)
  (:type :uint32)
  (:timestamp :uint32)
  (:window-id :uint32)
  (:state :uint8)
  (:repeat :uint8)
  (:padding2 :uint8)
  (:padding3 :uint8)
  (:keysym (:struct keysym)))

(%defstruct text-editing-event (:value %text-editing-event-value
                                :pointer %text-editing-event-pointer)
  (:type :uint32)
  (:timestamp :uint32)
  (:window-id :uint32)
  (:text :char :count 32)
  (:start :int32)
  (:length :int32))

(%defstruct text-input-event (:value %text-input-event-value
                              :pointer %text-input-event-pointer)
  (:type :uint32)
  (:timestamp :uint32)
  (:window-id :uint32)
  (:text :char :count 32))

(%defstruct mouse-motion-event (:value %mouse-motion-event-value
                                :pointer %mouse-motion-event-pointer)
  (:type :uint32)
  (:timestamp :uint32)
  (:window-id :uint32)
  (:which :uint32)
  (:state :uint32)
  (:x :int32)
  (:y :int32)
  (:xrel :int32)
  (:yrel :int32))

(%defstruct mouse-button-event (:value %mouse-button-event-value
                                :pointer %mouse-button-event-pointer)
  (:type :uint32)
  (:timestamp :uint32)
  (:window-id :uint32)
  (:which :uint32)
  (:button :uint8)
  (:state :uint8)
  (:padding1 :uint8)
  (:padding2 :uint8)
  (:x :int32)
  (:y :int32))

(%defstruct mouse-wheel-event (:value %mouse-wheel-event-value
                               :pointer %mouse-wheel-event-pointer)
  (:type :uint32)
  (:timestamp :uint32)
  (:window-id :uint32)
  (:which :uint32)
  (:x :int32)
  (:y :int32))

(%defstruct joy-axis-event (:value %joy-axis-event-value
                            :pointer %joy-axis-event-pointer)
  (:type :uint32)
  (:timestamp :uint32)
  (:which :int32)
  (:axis :uint8)
  (:padding1 :uint8)
  (:padding2 :uint8)
  (:padding3 :uint8)
  (:value :int16)
  (:padding4 :uint16))

(%defstruct joy-ball-event (:value %joy-ball-event-value
                            :pointer %joy-ball-event-pointer)
  (:type :uint32)
  (:timestamp :uint32)
  (:which :int32)
  (:ball :uint8)
  (:padding1 :uint8)
  (:padding2 :uint8)
  (:padding3 :uint8)
  (:xrel :int16)
  (:yrel :int16))

(%defstruct joy-hat-event (:value %joy-hat-event-value
                           :pointer %joy-hat-event-pointer)
  (:type :uint32)
  (:timestamp :uint32)
  (:which :int32)
  (:hat :uint8)
  (:value :uint8)
  (:padding1 :uint8)
  (:padding2 :uint8))

(%defstruct joy-button-event (:value %joy-button-event-value
                              :pointer %joy-button-event-pointer)
  (:type :uint32)
  (:timestamp :uint32)
  (:which :int32)
  (:button :uint8)
  (:state :uint8)
  (:padding1 :uint8)
  (:padding2 :uint8))

(%defstruct quit-event (:value %quit-event-value
                        :pointer %quit-event-pointer)
  (:type :uint32)
  (:timestamp :uint32))

(%defunion event (:value %event-value
                  :pointer %event-pointer)
  (:type :uint32)
  (:common common-event)
  (:window window-event)
  (:key keyboard-event)
  (:edit text-editing-event)
  (:text text-input-event)
  (:motion mouse-motion-event)
  (:button mouse-button-event)
  (:wheel mouse-wheel-event)
  (:jaxis joy-axis-event)
  (:jball joy-ball-event)
  (:jhat joy-hat-event)
  (:jbutton joy-button-event)
  (:quit quit-event)
  (:padding :uint8 :count 56))



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

(%defstruct color (:value color-value
                   :pointer color-pointer)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(%defstruct rect (:value %rect-value
                  :pointer %rect-pointer)
  (:x :int)
  (:y :int)
  (:w :int)
  (:h :int))

(%defstruct pixel-format (:value %pixel-format-value
                          :pointer %pixel-format-pointer)
  (:format :uint32)
  (:palette :pointer)
  (:bits-per-pixel :uint8)
  (:bytes-per-pixel :uint8)
  (:rmask :uint32)
  (:gmask :uint32)
  (:bmask :uint32)
  (:amask :uint32)
  (:rloss :uint8)
  (:gloss :uint8)
  (:bloss :uint8)
  (:aloss :uint8)
  (:rshift :uint8)
  (:gshift :uint8)
  (:bshift :uint8)
  (:ashift :uint8)
  (:refcount :int)
  (:next :pointer))

(%defstruct surface (:value %surface-value
                     :pointer %surface-pointer)
  (:flags :uint32)
  (:format :pointer)
  (:w :int)
  (:h :int)
  (:pitch :int)
  (:pixels :pointer)
  (:userdata :pointer)
  (:locked :int)
  (:lock-data :pointer)
  (:clip-rect (:struct rect))
  (:map :pointer)
  (:refcount :int))

(%defbitfield img-init-flags (:value %img-init-flags-value
                              :symbols %img-init-flags-symbols)
  (:jpg  #x00000001)
  (:png  #x00000002)
  (:tif  #x00000004)
  (:webp #x00000008))

(%defbitfield ttf-style-flags (:value %ttf-style-flags-value
                               :symbols %ttf-style-flags-symbols)
  (:bold          #x01)
  (:italic        #x02)
  (:underline     #x04)
  (:strikethrough #x08))

(%defenum ttf-hinting (:value %ttf-hinting-value
                       :symbol %ttf-hinting-symbol)
  (:normal 0)
  (:light 1)
  (:mono 2)
  (:none 3))

(%defbitfield mix-init-flags (:value %mix-init-flags-value
                              :symbols %mix-init-flags-symbols)
  (:flac       #x00000001)
  (:mod        #x00000002)
  (:modplug    #x00000004)
  (:mp3        #x00000008)
  (:ogg        #x00000010)
  (:fluidsynth #x00000020))

(%defenum audio-format (:value %audio-format-value
                        :symbol %audio-format-symbol)
  (:u8 #x0008)
  (:s8 #x8008)
  (:u16-lsb #x0010)
  (:s16-lsb #x8010)
  (:u16-msb #x1010)
  (:s16-msb #x9010)
  (:u16 #x0010)
  (:s16 #x8010))


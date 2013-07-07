
(in-package :mini-sdl2)

(defcfun ("SDL_SetMainReady" %set-main-ready) :void)
(defcfun ("SDL_Init" %init) :int (flags :uint32))
(defcfun ("SDL_Quit" %quit) :void)
(defcfun ("SDL_GetError" %get-error) :string)
(defcfun ("SDL_SetError" %set-error) :int (fmt :string) &rest)
(defcfun ("SDL_ClearError" %clear-error) :void)
(defcfun ("SDL_RWFromFile" %rw-from-file) :pointer (filename :string) (mode :string))
(defcfun ("SDL_RWClose" %rw-close) :int (context :pointer))
(defcfun ("SDL_JoystickEventState" %joystick-event-state) :int (state :int))

(defcfun ("IMG_Init" %img-init) :int (flags :int))
(defcfun ("IMG_Quit" %img-quit) :void)
(defcfun ("TTF_Init" %ttf-init) :int)
(defcfun ("TTF_Quit" %ttf-quit) :void)
(defcfun ("Mix_Init" %mix-init) :int (flags :int))
(defcfun ("Mix_Quit" %mix-quit) :void)

(defmacro with-sdl2 (flags &body body)
  `(unwind-protect
      ,(append
        (%list-no-nil
          'progn
          '(%set-main-ready)
          `(check-error (%init ,(%init-flags-value flags)))
          (if (member :video flags)
            `(%img-init ,(%img-init-flags-value '(:jpg :png :tif))))
          (if (member :video flags)
            `(%ttf-init))
          (if (member :audio flags)
            `(%mix-init ,(%mix-init-flags-value '(:flac :mod :mp3 :ogg))))
          (if (member :joystick flags)
            `(%joystick-event-state ,(%event-state-value :enable))))
        body)
      ,(%list-no-nil
        'progn
        (if (member :video flags) '(%img-quit))
        (if (member :video flags) '(%ttf-quit))
        (if (member :audio flags) '(%mix-quit))
        '(%quit))))

(defun check-error (errno)
  (when (< errno 0)
    (unwind-protect
      (error (get-error))
      (clear-error))))

(defun get-error () (%get-error))
(defun set-error (fmt &rest args) (%set-error (apply #'format nil fmt args)))
(defun clear-error () (%clear-error))

;; Internal Utilities
(defun %list-slot (&rest args)
  (loop for sym in args by #'cddr
        for frm in (cdr args) by #'cddr
        when sym collect (list sym frm)))

(defun %make-let (body &rest bindlist)
  `(let ,(apply #'%list-slot bindlist) ,@body))

(defmacro %with-rbop-from-file (spec &body body)
  (destructuring-bind (name path) spec
    `(let ((,name (%rw-from-file (translate-logical-pathname ,path) "rb")))
      (unwind-protect (progn ,@body)
                      (%rm-close ,name)))))

(export
  '(with-sdl2
    get-error
    set-error
    clear-error))



(in-package :mini-sdl2)

(defcfun ("IMG_Init" %img-init) :int (flags :int))
(defcfun ("IMG_Quit" %img-quit) :void)
(defcfun ("IMG_Load_RW" %img-load) :pointer (src :pointer) (freesrc :int))

(defcfun ("TTF_Init" %ttf-init) :int)
(defcfun ("TTF_Quit" %ttf-quit) :void)
(defcfun ("TTF_OpenFontRW" %ttf-open-font) :pointer (rwop :pointer) (freesrc :int) (ptsize :int))
(defcfun ("TTF_OpenFontIndexRW" %ttf-open-font-index) :pointer (rwop :pointer) (freesrc :int) (ptsize :int) (index :long))
(defcfun ("TTF_CloseFont" %ttf-close-font) :void (font :pointer))

(defcfun ("SDL_CreateWindow" %create-window) :pointer
  (title :string) (x :int) (y :int) (w :int) (h :int) (flags :uint32))
(defcfun ("SDL_DestroyWindow" %destroy-window) :void
  (win :pointer))
(defcfun ("SDL_GL_CreateContext" %create-context) :pointer
  (win :pointer))
(defcfun ("SDL_GL_DeleteContext" %delete-context) :void
  (context :pointer))
(defcfun ("SDL_GL_MakeCurrent" %make-current) :int 
  (window :pointer) (context :pointer))
(defcfun ("SDL_GL_SwapWindow" %swap-window) :void
  (window :pointer))

(defmacro with-window (winspec &body body)
  (destructuring-bind
    (name &key (title "Mini SDL2") (x 100) (y 100) (w 640) (h 480) flags)
    winspec
    `(let ((,name (%create-window ,title ,x ,y ,w ,h ,(%window-flags-value flags))))
      (unwind-protect (progn ,@body)
                      (%destroy-window ,name)))))
(defmacro with-context (name win &body body)
  `(let ((,name (%create-context ,win)))
    (unwind-protect (progn ,@body)
                     (%delete-context ,name))))
(defmacro with-window-and-context (spec &body body)
  (destructuring-bind (win ctx &rest winspec) spec
    `(with-window ,(cons win winspec) (with-context ,ctx ,win ,@body))))
(defmacro begin-frame (win ctx &body body)
  `(progn (%make-current ,win ,ctx)
          ,@body
          (%swap-window ,win)))

(export
  '(with-window
    with-context
    with-window-and-context
    begin-frame))


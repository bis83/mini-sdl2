
(in-package :mini-sdl2)

(defcfun ("IMG_Init" %img-init) :int
  (flags :int))
(defcfun ("IMG_Quit" %img-quit) :void)
(defcfun ("IMG_Load_RW" %img-load) :pointer
  (src :pointer)
  (freesrc :int))
(defcfun ("TTF_Init" %ttf-init) :int)
(defcfun ("TTF_Quit" %ttf-quit) :void)
(defcfun ("TTF_OpenFontRW" %ttf-open-font) :pointer
  (rwop :pointer)
  (freesrc :int)
  (ptsize :int))
(defcfun ("TTF_OpenFontIndexRW" %ttf-open-font-index) :pointer
  (rwop :pointer)
  (freesrc :int)
  (ptsize :int)
  (index :long))
(defcfun ("TTF_CloseFont" %ttf-close-font) :void
  (font :pointer))
(defcfun ("SDL_CreateWindow" %create-window) :pointer
  (title :string)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (flags :uint32))
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
(defcfun ("IMG_Load_RW" %img-load-rw) :pointer
  (src :pointer)
  (free :int))
(defcfun ("SDL_FreeSurface" %free-surface) :void
  (surface :pointer))
(defcfun ("TTF_OpenFontIndexRW" %ttf-open-font-rw) :pointer
  (src :pointer)
  (free :int)
  (ptsize :int)
  (index :long))
(defcfun ("TTF_CloseFont" %ttf-close-font) :void
  (font :pointer))
(defcfun ("TTF_SetFontStyle" %ttf-set-font-style) :void
  (font :pointer)
  (style :int))
(defcfun ("TTF_GetFontStyle" %ttf-get-font-style) :int
  (font :pointer))
(defcfun ("TTF_SetFontOutline" %ttf-get-font-outline) :void
  (font :pointer)
  (outline :int))
(defcfun ("TTF_GetFontOutline" %ttf-set-font-outline) :int
  (font :pointer))
(defcfun ("TTF_SetFontHinting" %ttf-set-font-hinting) :void
  (font :pointer)
  (hinting :int))
(defcfun ("TTF_GetFontHinting" %ttf-get-font-hinting) :int
  (font :pointer))
(defcfun ("TTF_SetFontKerning" %ttf-set-font-kerning) :void
  (font :pointer)
  (allowed :int))
(defcfun ("TTF_GetFontKerning" %ttf-get-font-kerning) :int
  (font :pointer))

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

(defmacro begin-frame (spec &body body)
  (destructuring-bind (win ctx &key no-swap) spec
    `(progn (%make-current ,win ,ctx)
            ,@body
            ,(if no-swap nil `(%swap-window ,win)))))

(defmacro %load-image (path)
  (let ((name (gensym)))
    `(%with-rbop-from-file (,name ,path)
      (%null-pointer-to-nil (%img-load-rw ,name 0)))))

(defmacro %close-image (image)
  `(%free-surface ,image))

(defmacro load-font (path size)
  (let ((name (gensym)))
    `(%with-rbop-from-file (,name ,path)
      (%null-pointer-to-nil (%ttf-open-fonti-index-rw ,name 0 ,size 0)))))

(defmacro close-font (font)
  `(%ttf-close-font ,font))

(defmacro font-attribute (attr font &rest value)
  (case attr
    ((:style)
      (if value `(%ttf-set-font-style ,font (%font-style-value ,(car value)))
                `(%font-style-symbols (%ttf-get-font-style ,font))))
    ((:outline)
      (if value `(%ttf-set-font-outline ,font ,(car value))
                `(%ttf-get-font-outline ,font)))
    ((:hinting)
      (if value `(%ttf-set-font-hinting ,font (%font-hinting-value ,(car value)))
                `(%font-hinting-symbol (%ttf-get-font-hinting ,font))))
    ((:kerning)
      (if value `(%ttf-set-font-kerning ,font ,(if (car value) 1 0))
                `(> (%ttf-get-dont-kerning ,font) 0)))))

(defmacro %render-text
  (mode font text &optional (fr 255) (fg 255) (fb 255) (fa 255)
                            (br 0) (bg 0) (bb 0) (ba 0))
  (case mode
    ((:solid)
      `(with-foreign-objects ((buf :uint8 255)
                              (col (:struct color)))
        (setf (%color-value ,col :r) ,fr)
        (setf (%color-value ,col :g) ,fg)
        (setf (%color-value ,col :b) ,fb)
        (setf (%color-value ,col :a) ,fa)
        (%ttf-render-utf8-solid ,font
                                ,(lisp-string-to-foreign text buf 255 :encoding :utf-8)
                                ,col)))
    ((:shaded)
      `(with-foreign-objects ((buf :uint8 255)
                              (fc (:struct color))
                              (bc (:struct color)))
        (setf (%color-value ,fc :r) ,fr)
        (setf (%color-value ,fc :g) ,fg)
        (setf (%color-value ,fc :b) ,fb)
        (setf (%color-value ,fc :a) ,fa)
        (setf (%color-value ,bc :r) ,br)
        (setf (%color-value ,bc :g) ,bg)
        (setf (%color-value ,bc :b) ,bb)
        (setf (%color-value ,bc :a) ,ba)
        (%ttf-render-utf8-shaded ,font
                                 ,(lisp-string-to-foreign text buf 255 :encoding :utf-8)
                                 ,fc ,bc)))
    ((:blended)
      `(with-foreign-objects ((buf :uint8 255)
                              (col (:struct color)))
        (setf (%color-value ,col :r) ,fr)
        (setf (%color-value ,col :g) ,fg)
        (setf (%color-value ,col :b) ,fb)
        (setf (%color-value ,col :a) ,fa)
        (%ttf-render-utf8-blended ,font
                                ,(lisp-string-to-foreign text buf 255 :encoding :utf-8)
                                ,col)))))

(export
  '(with-window
    with-context
    with-window-and-context
    begin-frame
    load-font
    close-font
    font-attribute))


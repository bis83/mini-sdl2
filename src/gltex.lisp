
(in-package :mini-sdl2)

;; OpenGL Bindings.
(defcfun ("glGenTextures" %gl-gen-textures) :void
  (n :uint)
  (textures :pointer))
(defcfun ("glBindTexture" %gl-bind-texture) :void
  (target :int)
  (texture :uint))
(defcfun ("glTexParameteri" %gl-tex-parameteri) :void
  (target :int)
  (pname :int)
  (param :int))
(defcfun ("glTexImage2D" %gl-tex-image2d) :void
  (target :int)
  (level :int)
  (internal-fmt :int)
  (width :uint)
  (height :uint)
  (border :int)
  (fmt :int)
  (type :int)
  (pixels :pointer))

(defconstant +gl-rgba+ #x1908)
(defconstant +gl-bgra+ #x80E1)
(defconstant +gl-rgb+ #x1907)
(defconstant +gl-bgr+ #x80E0)
(defconstant +gl-texture-2d+ #x0DE1)
(defconstant +gl-texture-min-filter+ #x2801)
(defconstant +gl-texture-mag-filter+ #x2800)
(defconstant +gl-linear+ #x2601)
(defconstant +gl-unsigned-byte+ #x1401)

(defun bind-gltex (image)
  (let* ((fmt (%surface-value image :format))
         (bytes-per-pixel (%pixel-format-value fmt :bytes-per-pixel))
         (rmask (%pixel-format-value fmt :rmask))
         (w (%surface-value image :w))
         (h (%surface-value image :h))
         (pixels (%surface-value image :pixels))
         (glfmt (case bytes-per-pixel
                  ((4) (if (= rmask #x000000ff) +gl-rgba+ +gl-bgra+))
                  ((3) (if (= rmask #x000000ff) +gl-rgb+ +gl-bgr+)))))
    (with-foreign-object (tex :uint)
      (%gl-gen-textures 1 tex)
      (%gl-bind-texture +gl-texture-2d+ (mem-ref tex :uint))
      (%gl-tex-parameteri +gl-texture-2d+ +gl-texture-min-filter+ +gl-linear+)
      (%gl-tex-parameteri +gl-texture-2d+ +gl-texture-mag-filter+ +gl-linear+)
      (%gl-tex-image2d
        +gl-texture-2d+ 0 bytes-per-pixel w h 0 glfmt +gl-unsigned-byte+ pixels)
      (mem-ref tex :uint))))

(export 'bind-gltex)


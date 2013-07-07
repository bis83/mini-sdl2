
(in-package :mini-sdl2)

(defcfun ("Mix_OpenAudio" %mix-open-audio) :int (freq :int) (format :uint16) (channels :int) (chunksize :int))
(defcfun ("Mix_CloseAudio" %mix-close-audio) :void)
(defcfun ("Mix_LoadWAV_RW" %mix-load-wav-rw) :pointer (src :pointer) (free :int))
(defcfun ("Mix_FreeChunk" %mix-free-chunk) :void (chunk :pointer))
(defcfun ("Mix_LoadMUS" %mix-load-mus) :pointer (filename :string))
(defcfun ("Mix_FreeMusic" %mix-free-music) :void (music :pointer))
(defcfun ("Mix_AllocateChannels" %mix-allocate-channels) :int (channels :int))
(defcfun ("Mix_VolumeMusic" %mix-volume-music) :int (volume :int))
(defcfun ("Mix_Volume" %mix-volume) :int (channel :int) (volume :int))
(defcfun ("Mix_FadeInMusic" %mix-fade-in-music) :int (music :pointer) (loops :int) (ms :int))
(defcfun ("Mix_PlayMusic" %mix-play-music) :int (music :pointer) (loops :int))
(defcfun ("Mix_FadeInChannelTimed" %mix-fade-in-channel) :int (channel :int) (chunk :pointer) (loops :int) (ms :int) (ticks :int))
(defcfun ("Mix_PlayChannelTimed" %mix-play-channel) :int (channel :int) (chunk :pointer) (loops :int) (ticks :int))
(defcfun ("Mix_PauseMusic" %mix-pause-music) :void)
(defcfun ("Mix_Pause" %mix-pause) :void (channel :int))
(defcfun ("Mix_ResumeMusic" %mix-resume-music) :void)
(defcfun ("Mix_Resume" %mix-resume) :void (channel :int))
(defcfun ("Mix_FadeOutMusic" %mix-fade-out-music) :int (ms :int))
(defcfun ("Mix_HaltMusic" %mix-halt-music) :int)
(defcfun ("Mix_FadeOutChannel" %mix-fade-out-channel) :int (channel :int) (ms :int))
(defcfun ("Mix_HaltChannel" %mix-halt-channel) :int (channel :int))
(defcfun ("Mix_PlayingMusic" %mix-playing-music) :boolean)
(defcfun ("Mix_Playing" %mix-playing) :int (channel :int))

(defmacro with-audio (spec &body body)
  (destructuring-bind (&key (frequency 22050) (format :s16) (channels 2) (chunksize 4096)) spec
    `(unwind-protect
      (progn (%mix-open-audio ,frequency ,(%audio-format-value format) ,channels ,chunksize)
             ,@body)
      (%mix-close-audio))))

(defmacro load-wave (path)
  (let ((name (gensym)))
    `(%with-rbop-from-file (,name ,path)
      (%null-pointer-to-nil (%mix-load-wav-rw ,name 0)))))

(defmacro close-sample (sample)
  `(%mix-free-chunk ,sample))

(defmacro load-music (path)
  `(%mix-load-mus (namestring (translate-logical-pathname ,path))))

(defmacro close-music (music)
  `(%mix-free-music ,music))

(defmacro channels (&optional size)
  `(%mix-allocate-channels ,(or size -1)))

(defmacro volume (&optional channel-or-music vol)
  (if (eq channel-or-music :music)
    `(%mix-volume-music ,(or vol -1))
    `(%mix-volume ,(or channel-or-music -1) ,(or vol -1))))

(defmacro play (channel-or-music sample-or-music &optional (loops 0) ms)
  (if (eq channel-or-music :music)
    (if ms `(%mix-fade-in-music ,sample-or-music ,loops ,ms)
           `(%mix-play-music ,sample-or-music ,loops))
    (if ms `(%mix-fade-in-channel ,(or channel-or-music -1) ,sample-or-music ,loops ,ms -1)
           `(%mix-play-channel ,(or channel-or-music -1) ,sample-or-music ,loops -1))))

(defmacro pause (&optional channel-or-music)
  (if (eq channel-or-music :music)
    `(%mix-pause-music)
    `(%mix-pause ,(or channel-or-music -1))))

(defmacro resume (&optional channel-or-music)
  (if (eq channel-or-music :music)
    `(%mix-resume-music)
    `(%mix-resume ,(or channel-or-music -1))))

(defmacro halt (&optional channel-or-music ms)
  (if (eq channel-or-music :music)
    (if ms `(%mix-fade-out-music ,ms)
           `(%mix-halt-music))
    (if ms `(%mix-fade-out-channel ,(or channel-or-music -1) ,ms)
           `(%mix-halt-channel ,(or channel-or-music -1)))))

(defmacro playing (&optional channel-or-music)
  (if (eq channel-or-music :music)
    `(%mix-playing-music)
    (if channel-or-music `(if (%mix-playing ,channel-or-music) t)
                         `(%mix-playing -1))))

(export
  '(with-audio
    load-wave
    close-sample
    load-music
    close-music
    channels
    volume
    play
    pause
    resume
    halt
    playing))


(in-package :mini-sdl2)

(defcfun ("Mix_OpenAudio" %mix-open-audio) :int (freq :int) (format :uint16) (channels :int) (chunksize :int))
(defcfun ("Mix_CloseAudio" %mix-close-audio) :void)

(defmacro with-audio (spec &body body)
  (destructuring-bind (&key (frequency 22050) (format :s16) (channels 2) (chunksize 4096)) spec
    `(unwind-protect
      (progn (%mix-open-audio ,frequency ,(%audio-format-value format) ,channels ,chunksize)
             ,@body)
      (%close-audio))))

(defmacro load-wave (path)
  nil)

(defmacro close-sample (sample)
  nil)

(defmacro load-music (path)
  nil)

(defmacro close-music (music)
  nil)

(defmacro channels (size)
  nil)

(defmacro volume (channel-or-music &optional vol)
  (if (eq channel-or-music :music)
    (if vol `(%mix-volume-music ,vol)
            `(%mix-volume-music -1))
    (if vol `(%mix-volume ,channel-or-music ,vol)
            `(%mix-volume ,channel-or-music -1))))

(defmacro play (channel-or-music sample-or-music &optional (loops 0) ms)
  (if (eq channel-or-music :music)
    (if ms `(%mix-fade-in-music ,sample-or-music ,loops ,ms)
           `(%mix-play-music ,sample-or-music ,loops))
    (if ms `(%mix-fade-in-channel ,channel-or-music ,sample-or-music ,loops ,ms)
           `(%mix-play-channel ,channel-or-music ,sample-or-music ,loops))))

(defmacro pause (channel-or-music)
  (if (eq channel-or-music :music)
    `(%mix-pause-music)
    `(%mix-pause ,channel-or-music)))

(defmacro resume (channel-or-music)
  (if (eq channel-or-music :music)
    `(%mix-resume-music)
    `(%mix-resume ,channel-or-music)))

(defmacro halt (channel-or-music &optional ms)
  (if (eq channel-or-music :music)
    (if ms `(%mix-fade-out-music ,ms)
           `(%mix-halt-music))
    (if ms `(%mix-fade-out-channel ,channel-or-music ,ms)
           `(%mix-halt-channel ,channel-or-music))))

(defmacro playing (channel-or-music)
  (if (eq channel-or-music :music)
    `(%mix-playing ,channel-or-music)
    `(%mix-playing-music)))

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

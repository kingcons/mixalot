(in-package :mixalot)

;;;; Basic stream protocol

(defclass streamer ()
  ((handle :reader streamer-handle :initarg :handle)
   (sample-rate :reader streamer-sample-rate :initarg :sample-rate)
   (output-rate :reader streamer-output-rate :initarg :output-rate)
   (length :reader streamer-length :initform nil)
   (position :reader streamer-position :initform 0)
   (song :accessor streamer-song :initarg :song)
   (stopped :accessor streamer-stopped :initform nil)
   (filename :initform nil :initarg :filename)
   (seek-to :initform nil))
  (:documentation "YEAHHHHHHH (file streamer)"))

(defgeneric streamer-mix-into (stream mixer buffer offset length time)
  (:documentation
   "Mix 'length' samples of stream output into buffer starting at 'offset'
 measured in samples, at 'time' (measured in samples since the mixer was
 created. The time measurement includes the offset, and is intended for
 synchronizing streams. Called from outside the mixer lock.")
  (:method ((stream function) mixer buffer offset length time)
    (funcall stream stream mixer buffer offset length time)))

(defgeneric streamer-write-into (stream mixer buffer offset length time)
  (:documentation
   "Write 'length' samples of stream output into buffer starting at
   'offset' (measured in samples), at 'time' (measured in samples
   since the mixer was created. The time measurement includes the
   offset, and is intended for synchronizing streams. The differs from
   stream-write-info in that you don't have to mix the data, the
   current contents are expected to be garbage and can be
   overwritten. Implementing this is optional. Called from outside the
   mixer lock.")
  (:method (stream mixer buffer offset length time)
    (declare (type sample-vector buffer)
             (type array-index offset length)
             (optimize (speed 3)))
    (fill buffer 0 :start offset :end (+ offset length))
    (streamer-mix-into stream mixer buffer offset length time)))

(defgeneric streamer-cleanup (stream mixer)
  (:documentation
  "Release resources and perform any other cleanups needed when a
  streamer is destroyed as a result of a call to mixer-remove-streamer.
  Called outside the mixer lock, so it's okay to manipulate the mixer.")
  (:method (stream mixer)
    (declare (ignore stream mixer))))

;;;; Pausing streams: The mixer handles pausing automatically.
;;;; Streamers need not define methods on these functions, unless they
;;;; have a reason to take action on pause/unpause.

(defgeneric streamer-pause (stream mixer)
  (:documentation "Pause playback of the streamer. A method on
  streamer-pause is optional and serves as a notification to the
  streamer that it has been paused; the default method is specialized
  on the mixer and can suspend playback without any special support
  from the streamer."))

(defgeneric streamer-unpause (stream mixer)
  (:documentation "Unpause playback of the streamer. A method on
  streamer-unpause is optional and serves as a notification to the
  streamer that it has been unpaused; the default method is
  specialized on the mixer and can resume playback without any special
  support from the streamer."))

(defgeneric streamer-paused-p (stream mixer)
  (:documentation "Query whether a stream is paused or not."))

(defgeneric streamer-length (stream)
  (:documentation "Returns length, in samples, of the audio stream, or
  NIL if it cannot be determined.")
  (:method (stream) nil))

(defgeneric streamer-position (stream)
  (:documentation "Returns current position within a seekable stream.")
  (:method (stream) nil))

;;;; Optional: Seekable stream protocol

(defgeneric streamer-seekable-p (stream mixer)
  (:documentation "Returns non-NIL if the streamer supports seeking.")
  (:method (stream mixer)
    (declare (ignore stream mixer))
    nil))

(defgeneric streamer-seek (stream mixer position &key &allow-other-keys)
  (:documentation "Seek to position (measured in samples) from the start of stream."))

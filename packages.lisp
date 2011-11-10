(defpackage :mixalot
  (:use :common-lisp :cffi :bordeaux-threads :mixalot-ffi-common)
  (:export #:alsa-error
           #:main-thread-init

           #:sample-vector
           #:stereo-sample
           #:mono-sample

           #:streamer
           #:streamer-mix-into
           #:streamer-write-into
           #:streamer-cleanup
           #:streamer-pause
           #:streamer-unpause
           #:streamer-paused-p
           #:streamer-seekable-p
           #:streamer-length
           #:streamer-seek
           #:streamer-position
           #:streamer-note-completion
           #:streamer-handle
           #:streamer-sample-rate
           #:streamer-output-rate
           #:streamer-song
           #:streamer-stopped

           ;; a bit uncomfortable with this but need it for all the with-slots
           ;; i.e. in mixalot-mp3, mixalot-vorbis, etc
           #:handle
           #:sample-rate
           #:output-rate
           #:length
           #:position
           #:seek-to

           #:mixer
           #:mixer-stream-lock
           #:mixer-stream-list
           #:mixer-current-time
           #:mixer-rate
           #:mixer-shutdown-flag
           #:mixer-add-streamer
           #:mixer-remove-streamer
           #:mixer-remove-all-streamers
           #:create-mixer
           #:destroy-mixer
           #:array-index
           #:with-array-pointer
           #:clamp-sample #:clamp-sample+
           #:mono->stereo #:stereo-left #:stereo-right
           #:%stereo-left #:%stereo-right
           #:split-sample
           #:mix-stereo-samples #:add-stereo-samples
           #:stereo-incf #:stereo-mixf
           #:make-test-streamer))

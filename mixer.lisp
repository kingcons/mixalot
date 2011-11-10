(in-package :mixalot)

;;;; Mixer process

;;; The mixer contains zero or more streamers and pumps samples from
;;; them to mix and send to the audio card. Streamers can be added and
;;; removed at (almost) any time.

;;; Another reasonable design would be that you connect a single
;;; stream to the audio device, and a mixer is just another type of
;;; stream. This would solve some problems for a certain kind of app,
;;; but I haven't pursued simply because I didn't think of it soon
;;; enough, and changing to that approach if/when I have use for it
;;; shouldn't break the API incompatibly.

(defstruct mixer
  (stream-lock (bordeaux-threads:make-lock "Mixer lock"))
  (stream-list  nil)
  (current-time 0)
  (rate         44100)
  (shutdown-flag nil)
  (stream-state (make-hash-table))
  device)

(defmacro with-mixer-lock ((mixer) &body body)
  `(with-lock-held ((mixer-stream-lock ,mixer))
    ,@body))

(defun mixer-add-streamer (mixer streamer)
  (with-mixer-lock (mixer)
    (cond
      ((mixer-shutdown-flag mixer)
       (error "You can't add a stream to a shutdown mixer!"))
      (t (push streamer (mixer-stream-list mixer))
         (values streamer (mixer-current-time mixer))))))

(defun %req-remove-streamer (mixer streamer)
  (setf (gethash streamer (mixer-stream-state mixer)) :remove))

(defun mixer-remove-streamer (mixer streamer)
  (with-mixer-lock (mixer)
    (%req-remove-streamer mixer streamer))
  (values))

(defun mixer-remove-all-streamers (mixer)
  (with-mixer-lock (mixer)
    (dolist (streamer (mixer-stream-list mixer))
      (%req-remove-streamer mixer streamer))))

;;; Obtaining a pointer to an array of unboxed data. I used to do this
;;; myself, but recentish CFFI can do it for me.
(defmacro with-array-pointer ((name array) &body body)
  `(cffi-sys:with-pointer-to-vector-data (,name ,array) ,@body))

#+NIL
(defmacro with-array-pointer ((name array) &body body)
  ;; Perhaps does the wrong thing for displaced arrays.
  ;; This will never affect me.
  ;; Also, SBCL gives a very bizarre code deletion warning here
  ;; when compiling the file in SLIME which goes away when I
  ;; compile just the definition.
  `((lambda (arrayoid body)
      (unless (typep arrayoid 'vector)
        (setf arrayoid (sb-kernel:%array-data-vector arrayoid)))
      (sb-sys:with-pinned-objects (arrayoid)
        (funcall body (sb-sys:vector-sap arrayoid))))
    ,array
    (lambda (,name) ,@body)))

(defmethod streamer-pause (stream (mixer mixer))
  (with-mixer-lock (mixer)
    (when (find stream (mixer-stream-list mixer))
      (setf (gethash stream (mixer-stream-state mixer)) :paused))))

(defmethod streamer-unpause (stream (mixer mixer))
  (with-mixer-lock (mixer)
    (when (eql (gethash stream (mixer-stream-state mixer)) :paused)
      (remhash stream (mixer-stream-state mixer)))))

(defmethod streamer-paused-p (stream (mixer mixer))
  (with-mixer-lock (mixer)
    (eql (gethash stream (mixer-stream-state mixer)) :paused)))

(defun update-playable (mixer playable-streams)
  (with-mixer-lock (mixer)
    (setf (fill-pointer playable-streams) 0)
    (dolist (stream (mixer-stream-list mixer))
      (let ((state (gethash stream (mixer-stream-state mixer))))
        (unless (eql :paused state)
          (vector-push-extend stream playable-streams))))))

(defun remove-removable (mixer temp-vector)
  (with-mixer-lock (mixer)
    (let ((state-table (mixer-stream-state mixer)))
      (setf (fill-pointer temp-vector) 0
            (mixer-stream-list mixer)
            (delete-if
             (lambda (streamer)
               (when (eql :remove (gethash streamer state-table))
                 (vector-push-extend streamer temp-vector)
                 (remhash streamer state-table)
                 t))
             (mixer-stream-list mixer)))))
  ;; Run the cleanups outside the lock:
  (loop for removed across temp-vector
        do (streamer-cleanup removed mixer)))

(defconstant +mixer-buffer-size+ 4096)
(deftype mixer-buffer-index () `(integer 0 ,+mixer-buffer-size+))

(defun run-mixer-process (mixer)
 (declare (optimize (speed 3)))
 (unwind-protect
  ;; Body
  (loop with time = 0
        with buffer-samples = +mixer-buffer-size+
        with buffer = (make-array buffer-samples :element-type '(unsigned-byte 32))
        with playable-streams = (make-array 0 :adjustable t :fill-pointer 0)
        with buffer-clear = nil
        until (mixer-shutdown-flag mixer)
        do
        ;; So that we don't have to hold the lock during the stream
        ;; callbacks, use this temporary vector:
        (remove-removable mixer playable-streams)
        (update-playable mixer playable-streams)
        ;; Loop through playable streams and generate audio
        (loop for streamer across playable-streams
              for first = t then nil
              as offset = 0             ; ...
              do
              (setf buffer-clear nil)
              (restart-case
                  (funcall (if first
                               #'streamer-write-into
                               #'streamer-mix-into)
                           streamer
                           mixer
                           buffer
                           offset
                           (- buffer-samples offset)
                           (+ time offset))
                (remove-streamer ()
                  :report "Delete this audio stream"
                  (mixer-remove-streamer mixer streamer))))
        ;; If there are no playable streams, we have to clear the buffer ourself.
        (when (and (zerop (length playable-streams))
                   (not buffer-clear))
          (fill buffer 0)
          (setf buffer-clear t))
        ;; Play the buffer.
        #-linux
        (let ((ret
               (with-array-pointer (ptr buffer)
                 (ao-play (mixer-device mixer) ptr (* 4 buffer-samples)))))
          (when (zerop ret)
            (format *trace-output* "libao error.")))

        #+linux
        (loop with offset of-type mixer-buffer-index = 0
              as nwrite = (- buffer-samples offset)
              as nframes = (with-array-pointer (ptr buffer)
                             (incf-pointer ptr (* offset 4))
                             (snd-pcm-writei (mixer-device mixer) ptr nwrite))
              do
              (unless (zerop offset)
                (format t "~&mixer time ~A partial offset ~:D~%"
                        (mixer-current-time mixer)
                        offset))
              (assert (integerp nframes))
              (cond
                ((< nframes 0)
                 (format *trace-output* "~&nframes<0, snd-pcm-recover")
                 (snd-pcm-recover (mixer-device mixer) nframes 1))
                ((< nframes nwrite)
                 (format *trace-output* "~&short write ~D vs ~D (offset ~D)~%"
                         nframes (- buffer-samples offset) offset)
                 (incf offset nframes))
                (t (loop-finish))))

        (incf time buffer-samples)
        (setf (mixer-current-time mixer) time))
   ;; Cleanup. After setting the shutdown flag, it is impossible to
   ;; add additional streamers, so there's no race during the shutdown.
   (with-mixer-lock (mixer) (setf (mixer-shutdown-flag mixer) t))
   (dolist (streamer (mixer-stream-list mixer))
     (streamer-cleanup streamer mixer))
   (clrhash (mixer-stream-state mixer))
   (setf (mixer-stream-list mixer) nil)))

(defun create-mixer (&key (rate 44100))
  "Create a new mixer at the specified sample rate, running in its own thread."
  (let ((mixer (make-mixer :rate rate)))
    (bordeaux-threads:make-thread
     (lambda ()
       #-linux
       (progn
         (setf (mixer-device mixer) (open-ao :rate rate))
         (run-mixer-process mixer))
       #+linux
       (call-with-pcm rate
        (lambda (pcm)
          (setf (mixer-device mixer) pcm)
          (run-mixer-process mixer))))
     :name (format nil "Mixer thread ~:D Hz" rate))
    mixer))

(defun destroy-mixer (mixer)
  (with-mixer-lock (mixer)
    (setf (mixer-shutdown-flag mixer) t))
  (values))

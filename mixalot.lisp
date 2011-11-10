;;;; Mixalot audio mixer for ALSA

;;;; Copyright (c) 2009,2010 Andy Hefner

;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sellcopies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject
;;;;  to the following conditions:

;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(in-package :mixalot)

(eval-when (:compile-toplevel)
  #-linux (pushnew 'use-ao *features*)
  #+linux (pushnew 'use-alsa *features*))

(deftype array-index ()
  #-sbcl '(integer 0 #.array-dimension-limit)
  #+sbcl 'sb-int:index)

(deftype stereo-sample () '(unsigned-byte 32))

(deftype mono-sample ()
  '(or
    (signed-byte 16)
    (unsigned-byte 16)))

(deftype sample-vector () '(simple-array stereo-sample 1))

;;;; FFI to minimal subset of ALSA library

#+mixalot::use-alsa
(define-foreign-library libasound
    (t (:or "libasound.so.2" "libasound.so"
            "/usr/lib/libasound.so"
            "/usr/local/lib/libasound.so")))

#+mixalot::use-alsa
(use-foreign-library libasound)

#+mixalot::use-alsa
(define-condition alsa-error (error)
  ((text :initarg :text))
  (:documentation "An error from the ALSA library")
  (:report
   (lambda (condition stream)
     (write-string (slot-value condition 'text) stream))))

#+mixalot::use-alsa
(defcfun snd-strerror :string (errnum :int))

#+mixalot::use-alsa
(defun check-error (circumstance result)
  (unless (zerop result)
    (error 'alsa-error
           :text (format nil "~A: ~A" circumstance (snd-strerror result)))))

#+mixalot::use-alsa
(defctype snd-pcm :pointer)

#+mixalot::use-alsa
(defcenum snd-pcm-stream
    (:playback 0)
    (:capture 1))

#+mixalot::use-alsa
(defcenum snd-pcm-mode
    (:blocking 0)
    (:nonblocking 1)
    (:async 2))

#+mixalot::use-alsa
(defcfun (%snd-pcm-open "snd_pcm_open") :int
  (pcm (:pointer snd-pcm))
  (name :string)
  (type snd-pcm-stream)
  (mode snd-pcm-mode))

#+mixalot::use-alsa
(defun snd-pcm-open (name stream-type mode)
  (with-foreign-object (pcm 'snd-pcm)
    (check-error
     (format nil "PCM open of ~W (~A,~A)" name stream-type mode)
     (%snd-pcm-open pcm name stream-type mode))
    (validate-pointer (mem-ref pcm 'snd-pcm))))

#+mixalot::use-alsa
(defcfun snd-pcm-close :int
  (pcm snd-pcm))

#+mixalot::use-alsa
(defcenum snd-pcm-format
  (:snd-pcm-format-s16-le 2))

#+mixalot::use-alsa
(defcenum snd-pcm-access
    (:snd-pcm-access-rw-interleaved 3)
    (:snd-pcm-access-rw-noninterleaved 4))

#+mixalot::use-alsa
(defcfun snd-pcm-set-params :int
  (pcm           snd-pcm)
  (format        snd-pcm-format)
  (access        snd-pcm-access)
  (channels      :unsigned-int)
  (rate          :unsigned-int)
  (soft-resample :int)
  (latency       :unsigned-int))

#+mixalot::use-alsa
(defcfun snd-pcm-recover :int
  (pcm    snd-pcm)
  (err    :int)
  (silent :int))

#+mixalot::use-alsa (defctype snd-pcm-sframes :long)
#+mixalot::use-alsa (defctype snd-pcm-uframes :unsigned-long)

#+mixalot::use-alsa
(defcfun snd-pcm-writei snd-pcm-sframes
  (pcm    snd-pcm)
  (buffer :pointer)
  (size   snd-pcm-uframes))

#+mixalot::use-alsa (defctype snd-output :pointer)

#+mixalot::use-alsa
(defcfun snd-output-stdio-attach :int
  (outputp (:pointer snd-output))
  (file    :pointer)
  (close   :int))

#+mixalot::use-alsa
(defcfun snd-pcm-dump :int
  (pcm snd-pcm)
  (out snd-output))

#+mixalot::use-alsa
(defun main-thread-init ()
  ;; libao needs this. ALSA does not, which is the only good thing I can say about it.
  (values))

;;;; ALSA Utilities

#+mixalot::use-alsa (defcvar stdout :pointer)

#+mixalot::use-alsa
(defun dump-pcm-info (pcm)
  (with-foreign-object (output :pointer)
    (check-error
     "Attach output"
     (snd-output-stdio-attach output stdout 0))
    (check-error
     "PCM diagnostic state dump"
     (snd-pcm-dump pcm (mem-ref output :pointer)))))

#+mixalot::use-alsa
(defun call-with-pcm (rate continuation)
  (let ((pcm (snd-pcm-open "default" :playback :blocking)))
    (unwind-protect
         (progn
           (check-error
            "PCM set parameters"
            (snd-pcm-set-params
             pcm :snd-pcm-format-s16-le :snd-pcm-access-rw-interleaved
             2 rate 1 300000))
           (funcall continuation pcm))
      (snd-pcm-close pcm))))

;;;; Alternate interface using libao on OS X.

;;; This isn't ideal. You're forced to initialize the audio system
;;; (and thus the mixer) from the "main thread", due to OS X being a
;;; half-assed joke.

;;; In theory this could work in Win32 as well. I haven't tried it.

#+mixalot::use-ao
(define-foreign-library libao
  (:darwin (:or "libao.4.dylib" "/opt/local/lib/libao.4.dylib"))
  (t (:or "libao.so")))

#+mixalot::use-ao (use-foreign-library libao)

;; Danger! This must be called from the main thread! Stupid OS X.
#+mixalot::use-ao
(defcfun ao-initialize :void)

#+mixalot::use-ao
(defcfun ao-default-driver-id :int)

#+mixalot::use-ao
(defcstruct (ao-sample-format :conc-name ao-fmt-)
  (bits :int)
  (rate :int)
  (channels :int)
  (byte-format :int)
  (matrix :string))

#+mixalot::use-ao (defconstant AO_FMT_LITTLE 1)
#+mixalot::use-ao (defconstant AO_FMT_BIG    2)
#+mixalot::use-ao (defconstant AO_FMT_NATIVE 4)

#+mixalot::use-ao (defctype ao-device* :pointer)

#+mixalot::use-ao
(defcfun ao-open-live ao-device*
  (driver-id :int)
  (format (:pointer ao-sample-format))
  (options :pointer))

#+mixalot::use-ao (defvar *ao-main-thread-init* nil)
#+mixalot::use-ao (defvar *aodev* nil)

#+mixalot::use-ao
(defun open-ao (&key (rate 44100))
  (unless *ao-main-thread-init*
    (error "libao not initialized. You must call MIXALOT:MAIN-THREAD-INIT from the main thread of your lisp. In SBCL, this will be the initial REPL (the *inferior-lisp* buffer in SLIME). If you call it from another thread, Lisp may crash."))
  (with-foreign-object (fmt 'ao-sample-format)
    (with-foreign-string (matrix "L,R")
     (setf (ao-fmt-bits fmt) 16
           (ao-fmt-channels fmt) 2
           (ao-fmt-rate fmt) rate
           (ao-fmt-byte-format fmt) AO_FMT_LITTLE
           (ao-fmt-matrix fmt) matrix)
    (ao-open-live (ao-default-driver-id)
                   fmt
                   (null-pointer)))))

#+mixalot::use-ao
(defun main-thread-init ()
  (unless *ao-main-thread-init*
    (setf *ao-main-thread-init* t)
    (ao-initialize)))

#+mixalot::use-ao
(defcfun ao-play :int
  (device ao-device*)
  (output-samples :pointer)
  (num-bytes :uint32))


;;;; Fastish sample manipulation

(declaim (inline stereo-sample sign-extend-16
                 clamp-sample clamp-sample+
                 mono->stereo stereo-left stereo-right
                 %stereo-left %stereo-right
                 split-sample
                 mix-stereo-samples add-stereo-samples
                 scale-sample scale-stereo-sample))

(defun stereo-sample (left right)
  (declare (optimize (speed 3))
           (type mono-sample left right))
  (logior (ldb (byte 16 0) left)
          (ash (ldb (byte 16 0) right) 16)))

(defun mono->stereo (sample) (stereo-sample sample sample))

(defun sign-extend-16 (x)
  (declare (optimize (speed 3))
           (type (unsigned-byte 16) x))
  (let ((c (ash -1 15)))
    (logxor (+ x c) c)))

(defun %stereo-left (sample)
  (declare (optimize (speed 3)) (type stereo-sample sample))
  (ldb (byte 16 0)  sample))

(defun %stereo-right (sample)
  (declare (optimize (speed 3)) (type stereo-sample sample))
  (ldb (byte 16 16)  sample))

(defun stereo-left (sample)
  (declare (optimize (speed 3)) (type stereo-sample sample))
  (sign-extend-16 (%stereo-left  sample)))

(defun stereo-right (sample)
  (declare (optimize (speed 3)) (type stereo-sample sample))
  (sign-extend-16 (%stereo-right sample)))

(defun split-sample (sample)
  (values (stereo-left sample) (stereo-right sample)))

(defun clamp-sample (x) (min 32767 (max -32768 x)))
(defun clamp-sample+ (x y) (clamp-sample (+ x y)))

(defun mix-stereo-samples (x y)
  "Mix two stereo samples by clamped addition"
  (declare (optimize (speed 3)) (type stereo-sample x y))
  (stereo-sample (clamp-sample+ (stereo-left  x) (stereo-left  y))
                 (clamp-sample+ (stereo-right x) (stereo-right y))))

(defun add-stereo-samples (x y)
  "Add two stereo samples, without clipping."
  (declare (optimize (speed 3)) (type stereo-sample x y))
  (logior (logand #xFFFF (+ x y))
          (logand #xFFFF0000 (+ x (logand #xFFFF0000 y))))
  #+NIL ;; Equivalent, slower version:
  (stereo-sample (logand #xFFFF (+ (%stereo-left x) (%stereo-left y)))
                 (logand #xFFFF (+ (%stereo-right x) (%stereo-right y)))))

(defun scale-sample (x y)
  (declare (optimize (speed 3))
           (type mono-sample x y))
  (ash (* x y) -16))

(defun scale-stereo-sample (stereo scale)
  (declare (optimize (speed 3))
           (type stereo-sample stereo)
           (type (signed-byte 16) scale))
  #+NIL
  (logior (logand #xFFFF0000 (* scale (ash stereo -16)))
          (logand #x0000FFFF (ash (* (logand stereo #xFFFF) scale) -16)))

  (stereo-sample (scale-sample (stereo-left  stereo) scale)
                 (scale-sample (stereo-right stereo) scale)))

(define-modify-macro stereo-incf (sample) add-stereo-samples)
(define-modify-macro stereo-mixf (sample) mix-stereo-samples)

;;;; Testing streamer

(defun make-test-streamer ()
  (let ((n 0)
        (phase 0.0))
    (lambda (streamer mixer buffer offset length time)
      (declare (ignore time))
      (loop for index upfrom offset
            repeat length
            with freq = (+ 200 (* n 200))
            with dp = (* 2.0 pi freq 1/44100)
            as sample = (round (* 5000 (sin phase)))
            do
            (stereo-incf (aref buffer index) (mono->stereo sample))
            (incf phase dp))
      (incf n)
      (when (= n 6)
        (mixer-remove-streamer mixer streamer)))))

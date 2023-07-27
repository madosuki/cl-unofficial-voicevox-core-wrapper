(in-package :cl-user)
(defpackage unofficial-voicevox-core-wrapper-cl
  (:use :cl :cffi)
  (:import-from :unofficial-voicevox-core-wrapper-cl.types
                :uint32
                :uint16
                :voicevox-result-code-type
                :voicevox-acceleration-mode-type)
  (:export
   :initialize
   :generate-wav
   :generate-audio-query
   :load-library
   :get-version
   :is-gpu-mode
   ))
(in-package :unofficial-voicevox-core-wrapper-cl)

(cffi:defcenum voicevox-result-code
  (:voicevox-result-ok 0)
  (:voicevox-result-not-loaded-openjtalk-dict-error 1)
  (:voicevox-result-load-model-error 2)
  (:voicevox-result-get-supported-device-error 3)
  (:voicevox-result-gpu-support-error 4)
  (:voicevox-result-load-metas-error 5)
  (:voicevox-result-uninitialized-status-error 6)
  (:voicevox-result-invalid-speaker-id-error 7)
  (:voicevox-result-invalid-model-index-error 8)
  (:voicevox-result-inference-error 9)
  (:voicevox-result-extract-full-context-label-error 10)
  (:voicevox-result-invalid-utf8-input-error 11)
  (:voicevox-result-parse-kana-error 12)
  (:voicevox-result-invalid-audio-query-error 13))

(cffi:defcenum voicevox-acceleration-mode-enum
  (:voicevox-acceleration-mode-auto 0)
  (:voicevox-acceleration-mode-cpu 1)
  (:voicevox-acceleration-mode-gpu 2))

(cffi:defcstruct voicevox-audio-query-options
  (kana :int))

(cffi:defcstruct voicevox-tts-options
  (kana :bool)
  (enable-interrogative-upspeak :bool))

(cffi:defcstruct voicevox-initialize-options
  (acceleration_mode voicevox-acceleration-mode-enum)
  (cpu_num_threads :uint16)
  (load_all_models :bool)
  (open_jtalk_dict_dir (:pointer :char)))


(cffi:defcfun ("voicevox_initialize" vv-initialize) :int
  "initialize for voicevox core"
  (options (:struct voicevox-initialize-options)))
(declaim (ftype (function (&key (:acceleration-mode voicevox-acceleration-mode-type)
                                (:cpu-num-threads uint16)
                                (:load-all-models boolean)
                                (:open-jtalk-dict-dir string))
                          (values voicevox-result-code-type &optional))
                initialize))
(defun initialize (&key acceleration-mode cpu-num-threads load-all-models open-jtalk-dict-dir)
  (let ((options-ini (cffi:foreign-alloc '(:struct voicevox-initialize-options)))
        (open-jtalk-dict-dir-path-c (cffi:foreign-string-alloc open-jtalk-dict-dir
                                                               :encoding :UTF-8)))
    (setf (cffi:foreign-slot-value options-ini '(:struct voicevox-initialize-options) 'acceleration_mode) acceleration-mode
          (cffi:foreign-slot-value options-ini '(:struct voicevox-initialize-options) 'cpu_num_threads) cpu-num-threads
          (cffi:foreign-slot-value options-ini '(:struct voicevox-initialize-options) 'load_all_models) load-all-models
          (cffi:foreign-slot-value options-ini '(:struct voicevox-initialize-options) 'open_jtalk_dict_dir) open-jtalk-dict-dir-path-c)
    (let ((error-status
            (get-error-from-code (vv-initialize (cffi:mem-aref options-ini '(:struct voicevox-initialize-options))))))
      (free-for-foreign
       options-ini)
      (cffi:foreign-string-free open-jtalk-dict-dir-path-c)
      error-status)))


(cffi:defcfun ("voicevox_make_default_initialize_options" vv-make-default-initialize-options) (:struct voicevox-initialize-options))

(cffi:defcfun ("voicevox_is_gpu_mode" vv-is-gpu-mode) :bool)
(defun is-gpu-mode ()
  (vv-is-gpu-mode))

(cffi:defcfun ("voicevox_get_version" vv-get-version) :string)
(defun get-version ()
  (vv-get-version))


(cffi:defcfun ("voicevox_audio_query" vv-audio-query) :int
  "Do audio query"
  (text (:pointer :char))
  (speaker-id :uint32)
  (options (:struct voicevox-audio-query-options))
  (output-audio-query-json (:pointer (:pointer :char))) ;; this pointer is **char
  )

(cffi:defcfun ("voicevox_make_default_tts_options" vv-make-default-tts-options) (:struct voicevox-tts-options))
(cffi:defcfun ("voicevox_tts" vv-tts) :int
  (text (:pointer :char))
  (speaker-id :uint32)
  (options (:struct voicevox-tts-options))
  (output-wav-length (:pointer :uint))
  (output-wav (:pointer (:pointer :char))))

(defun make-array-from-wav-bytes (wav-bytes length)
  (print (type-of wav-bytes))
  (let ((result (make-array length)))
    (loop for i from 0 below length
          do
             (let ((tmp (cffi:mem-aref (cffi:mem-aref wav-bytes '(:pointer (:pointer :uint8)) 0)
                                       :uint8
                                       i)))
               (setf (aref result i) tmp)))
    result))

(declaim (inline get-error-from-code))
(defun get-error-from-code (code)
  (cffi:foreign-enum-keyword 'voicevox-result-code code))

(defun free-for-foreign (&rest targets)
  (dolist (x targets)
    (cffi:foreign-free x)))

(declaim (ftype (function (&key
                           (:text string)
                           (:kana boolean)
                           (:speaker-id uint32))
                          list)
                generate-audio-query))
(defun generate-audio-query (&key text kana speaker-id)
  (let ((audio-query-options (cffi:foreign-alloc '(:struct voicevox-audio-query-options)))
        (out-audio-query-json (cffi:foreign-alloc '(:pointer (:pointer :char))))
        (text-c (cffi:foreign-string-alloc text))
        (result nil))
    (setf (cffi:foreign-slot-value audio-query-options '(:struct voicevox-audio-query-options) 'kana) (if kana 1 0))
    (let ((error-status (get-error-from-code
                         (vv-audio-query
                          text-c
                          speaker-id
                          (cffi:mem-ref audio-query-options '(:struct voicevox-audio-query-options))
                          out-audio-query-json))))
      (if (equal error-status :voicevox-result-ok)
          (setq result (jojo:parse (cffi:foreign-string-to-lisp (cffi:mem-aref out-audio-query-json '(:pointer (:pointer :char))))))
          (setq result error-status))
      (free-for-foreign
       audio-query-options
       out-audio-query-json)
      (cffi:foreign-string-free text-c)
      (list :error-status error-status :audio-query result))))

(declaim (ftype (function (&key
                           (:text string )
                           (:kana boolean)
                           (:enable-interrogative-upspeak boolean)
                           (:speaker-id uint32))
                          (values list &optional))
                generate-wav))
(defun generate-wav (&key text kana enable-interrogative-upspeak speaker-id)
  (let ((options-tts (cffi:foreign-alloc '(:struct voicevox-tts-options)))
        (wav-length (cffi:foreign-alloc :uintptr
                                        :initial-element 0))
        (out-wav-bytes (cffi:foreign-alloc '(:pointer (:pointer :uint8))))
        (text-c (cffi:foreign-string-alloc text))
        (error-status :voicevox-result-ok))
    (setf
     (cffi:foreign-slot-value options-tts '(:struct voicevox-tts-options) 'kana) kana
     (cffi:foreign-slot-value options-tts '(:struct voicevox-tts-options) 'enable-interrogative-upspeak) enable-interrogative-upspeak)
    (setq error-status
          (get-error-from-code
           (vv-tts
            text-c
            speaker-id
            (cffi:mem-ref options-tts '(:struct voicevox-tts-options))
            wav-length
            out-wav-bytes
            )))
    (let* ((wav-length-unrefed (cffi:mem-ref wav-length :uint))
           (wav-bytes-array (make-array-from-wav-bytes out-wav-bytes wav-length-unrefed)))
      (free-for-foreign
       options-tts
       wav-length
       out-wav-bytes)
      (cffi:foreign-string-free text-c)
      (list :error-status error-status :wav-length wav-length-unrefed :wav-bytes wav-bytes-array))))



(defun load-library (path)
  (cffi:load-foreign-library path))


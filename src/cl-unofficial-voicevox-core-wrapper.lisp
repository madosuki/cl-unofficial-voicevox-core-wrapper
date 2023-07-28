(in-package :cl-user)
(defpackage cl-unofficial-voicevox-core-wrapper
  (:use :cl :cffi)
  (:nicknames :unofficial-vv-core-wrapper)
  (:import-from
   :cl-unofficial-voicevox-core-wrapper.types
   :uint32
   :uint16
   :voicevox-result-code-type
   :voicevox-acceleration-mode-type)
  (:export
   :voicevox-result-code-type
   :voicevox-acceleration-mode-type
   #:initialize
   #:tts
   #:audio-query
   #:load-library
   #:load-model
   #:get-version
   #:is-gpu-mode
   #:load-model
   #:is-model-loaded
   #:finalize
   #:get-supported-version
   #:get-metas-json
   #:predict-duration
   #:predict-intonation
   #:decode
   #:synthesis))
(in-package :cl-unofficial-voicevox-core-wrapper)

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

(defcfun ("voicevox_error_result_to_message" vv-error-result-to-message) :string
  (result-code voicevox-result-code))

(declaim (inline get-result-from-code))
(defun get-result-from-code (code)
  (cffi:foreign-enum-keyword 'voicevox-result-code code))

(cffi:defcstruct voicevox-audio-query-options
    (kana :int))

(cffi:defcstruct voicevox-tts-options
  (kana :bool)
  (enable-interrogative-upspeak :bool))

(cffi:defcstruct voicevox-synthesis-options
  (enable-interrogative-upspeak :int))
(cffi:defcfun ("voicevox_make_default_synthesis_options" vv-make-default-synthesis-options) (:struct voicevox-synthesis-options))

(cffi:defcstruct voicevox-initialize-options
  (acceleration_mode voicevox-acceleration-mode-enum)
  (cpu_num_threads :uint16)
  (load_all_models :bool)
  (open_jtalk_dict_dir (:pointer :char)))

(cffi:defcfun ("voicevox_make_default_initialize_options" vv-make-default-initialize-options) (:struct voicevox-initialize-options))

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
  (cffi:with-foreign-objects ((options-ini '(:struct voicevox-initialize-options)))
    (cffi:with-foreign-strings ((open-jtalk-dict-dir-path-c open-jtalk-dict-dir))
      (setf (cffi:foreign-slot-value options-ini '(:struct voicevox-initialize-options) 'acceleration_mode) acceleration-mode
            (cffi:foreign-slot-value options-ini '(:struct voicevox-initialize-options) 'cpu_num_threads) cpu-num-threads
            (cffi:foreign-slot-value options-ini '(:struct voicevox-initialize-options) 'load_all_models) load-all-models
            (cffi:foreign-slot-value options-ini '(:struct voicevox-initialize-options) 'open_jtalk_dict_dir) open-jtalk-dict-dir-path-c)
      (get-result-from-code (vv-initialize (cffi:mem-aref options-ini '(:struct voicevox-initialize-options)))))))

(cffi:defcfun ("voicevox_is_gpu_mode" is-gpu-mode) :bool)
(cffi:defcfun ("voicevox_get_version" get-version) :string)
(cffi:defcfun ("voicevox_load_model" load-model) :int
  (speaker-id :uint32))
(cffi:defcfun ("voicevox_is_model_loaded" is-model-loaded) :bool
  (speaker-id :uint32))
(cffi:defcfun ("voicevox_finalize" finalize) :void)
(cffi:defcfun ("voicevox_get_metas_json" get-metas-json) :string)
(cffi:defcfun ("voicevox_get_supported_device_json" get-supported-device-json) :string)
(cffi:defcfun ("voicevox_wav_free" vv-wav-free) :void
  (wav (:pointer :uint8)))


(cffi:defcfun ("voicevox_audio_query_json_free" vv-audio-query-json-free) :void
  (audio-query-json (:pointer :char)))
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
  (output-wav-length (:pointer :uintptr))
  (output-wav (:pointer (:pointer :char))))

(defun make-array-from-pointer (target length pointer-type value-type)
  (let ((result (make-array length)))
    (loop for i from 0 below length
          do
             (let ((tmp (cffi:mem-aref (cffi:mem-aref target pointer-type)
                                       value-type
                                       i)))
               (setf (aref result i) tmp)))
    result))


(defun free-for-foreign (&rest targets)
  (dolist (x targets)
    (cffi:foreign-free x)))

(declaim (ftype (function (&key
                           (:text string)
                           (:kana boolean)
                           (:speaker-id uint32))
                          list)
                audio-query))
(defun audio-query (&key text kana speaker-id)
  (cffi:with-foreign-objects ((audio-query-options '(:struct voicevox-audio-query-options))
                              (out-audio-query-json '(:pointer :char)))
    (cffi:with-foreign-strings ((text-c text))
      (setf (cffi:foreign-slot-value audio-query-options '(:struct voicevox-audio-query-options) 'kana) (if kana 1 0))
      (let ((result-status (get-result-from-code
                            (vv-audio-query
                             text-c
                             speaker-id
                             (cffi:mem-ref audio-query-options '(:struct voicevox-audio-query-options))
                             out-audio-query-json))))
        (if (equal result-status :voicevox-result-ok)
            (let ((result (cffi:foreign-string-to-lisp (cffi:mem-aref out-audio-query-json '(:pointer :char)))))
              (list :result-status result-status :audio-query result))
            (list :result-status result-status))))))

(declaim (ftype (function (&key
                           (:text string )
                           (:kana boolean)
                           (:enable-interrogative-upspeak boolean)
                           (:speaker-id uint32))
                          (values list &optional))
                tts))
(defun tts (&key text kana enable-interrogative-upspeak speaker-id)
  (cffi:with-foreign-objects ((options-tts '(:struct voicevox-tts-options))
                              (wav-length :uintptr)
                              (out-wav-bytes (:pointer :uint8)))
    (cffi:with-foreign-strings ((text-c text))
      (setf
       (cffi:foreign-slot-value options-tts '(:struct voicevox-tts-options) 'kana) kana
       (cffi:foreign-slot-value options-tts '(:struct voicevox-tts-options) 'enable-interrogative-upspeak) enable-interrogative-upspeak)
      (let ((result-status
              (get-result-from-code
               (vv-tts
                text-c
                speaker-id
                (cffi:mem-ref options-tts '(:struct voicevox-tts-options))
                wav-length
                out-wav-bytes
                ))))
        (if (eq result-status :voicevox-result-ok)
            (let* ((wav-length-unref (cffi:mem-ref wav-length :uint))
                   (wav-bytes-array (make-array-from-pointer
                                     out-wav-bytes
                                     wav-length-unref
                                     (:pointer :uint8)
                                     :uint8)))
              (list :result-status result-status :wav-length wav-length-unref :wav-bytes wav-bytes-array))
            (list :result-status result-status))))))



(defun load-library (path)
  (cffi:load-foreign-library path))


(defcfun ("voicevox_predict_duration_data_free" vv-predict-duration-data-free) :void
  (predict-duration-data (:pointer :float)))
(defcfun ("voicevox_predict_duration" vv-predict-duration) :int
  (length :uintptr)
  (phoneme-vector (:pointer :int64))
  (speaker-id :uint32)
  (output-predict-durarion-data-length (:pointer :uintptr))
  (output-predict-durarion-data (:pointer (:pointer :float))))
(defun predict-duration (length phoneme-vector speaker-id)
  (cffi:with-foreign-objects ((arr :int64 length)
                              (output-predict-durarion-data-length :uintptr)
                              (output-predict-durarion-data (:pointer :float)))
    (loop for i from 0 below length
          do (setf (cffi:mem-aref arr :int64 i) (aref phoneme-vector i)))
    (let ((result-status (get-result-from-code (vv-predict-duration
                                                length
                                                arr
                                                output-predict-durarion-data-length
                                                output-predict-durarion-data))))
      (if (eq result-status :voicevox-result-ok)
          (let ((output-predict-durarion-data-length-unref (cffi:mem-ref output-predict-durarion-data-length :uintptr))
                (output-predict-durarion-data-lisp-array (make-array-from-pointer
                                                          output-predict-durarion-data
                                                          output-predict-durarion-data-length-unref
                                                          (:pointer :float)
                                                          :float)))
            (list :result-status result-status
                  :predict-duration-data-length output-predict-durarion-data-length-unref
                  :predict-duration-data output-predict-durarion-data-lisp-array))
          (list :result-status result-status)))))

(defcfun ("voicevox_predict_intonation_data_free" vv-predict-intonation-data-free) :void
  (predict-intonation-data (:pointer :float)))
(defcfun ("voicevox_predict_intonation" vv-predict-intonation) :int
  (length :uintptr)
  (vowel-phoneme-vector (:pointer :int64))
  (consonant-phoneme-vector (:pointer :int64))
  (start-accent-vector (:pointer :int64))
  (end-accent-vector (:pointer :int64))
  (start-accent-phrase-vector (:pointer :int64))
  (end-accent-phrase-vector (:pointer :int64))
  (speaker-id :uint32)
  (output-predict-intonation-data-length :uintptr)
  (output-predict-intonation-data (:pointer (:pointer :float))))
(defun predict-intonation (length
                           vowel-phoneme-vector
                           consonant-phoneme-vector
                           start-accent-vector
                           end-accent-vector
                           start-accent-phrase-vector
                           end-accent-phrase-vector
                           speaker-id)
  (cffi:with-foreign-objects ((c-vowel-phoneme-vector :int64 length)
                              (c-consonant-phoneme-vector :int64 length)
                              (c-start-accent-vector :int64 length)
                              (c-end-accent-vector :int64 length)
                              (c-start-accent-phrase-vector :int64 length)
                              (c-end-accent-phrase-vector :int64 length)
                              (output-predict-intonation-data-length :uintptr)
                              (output-predict-intonation-data (:pointer :float)))
    (loop for i from 0 below length
          do (setf
              (cffi:mem-aref c-vowel-phoneme-vector :int64 i) (aref vowel-phoneme-vector i)
              (cffi:mem-aref c-consonant-phoneme-vector :int64 i) (aref consonant-phoneme-vector i)
              (cffi:mem-aref c-start-accent-vector :int64 i) (aref start-accent-vector i)
              (cffi:mem-aref c-end-accent-vector :int64 i) (aref end-accent-vector i)
              (cffi:mem-aref c-start-accent-phrase-vector :int64 i) (aref start-accent-phrase-vector i)
              (cffi:mem-aref c-end-accent-phrase-vector ':int64 i) (aref end-accent-phrase-vector i)))
    (let ((result-status
            (get-result-from-code
             (vv-predict-intonation
              length
              c-vowel-phoneme-vector
              c-consonant-phoneme-vector
              c-start-accent-vector
              c-end-accent-vector
              c-start-accent-phrase-vector
              c-end-accent-phrase-vector
              output-predict-intonation-data-length
              output-predict-intonation-data))))
      (if (eq result-status :voicevox-result-ok)
          (let* ((output-predict-intonation-data-length-unref (cffi:mem-ref output-predict-intonation-data-length :uintptr))
                 (predict-intonation-data-lisp-array (make-array-from-pointer
                                                      output-predict-intonation-data
                                                      output-predict-intonation-data-length-unref
                                                      (:pointer :float)
                                                      :float)))
            (list :result-status result-status
                  :predict-intonation-data predict-intonation-data-lisp-array
                  :predict-intonation-data-length output-predict-intonation-data-length-unref))
          (list :result-status result-status)))))

(cffi:defcfun ("voicevox_decode_data_free" vv-decode-data-free) :void
  (decode-data (:pointer :float)))
(cffi:defcfun ("voicevox_decode" vv-decode) :int
  (length :uintptr)
  (phoneme-size :uintptr)
  (f0 (:pointer :float))
  (phoneme-vector (:pointer :float))
  (speaker-id :uint32)
  (output-decode-data-length (:pointer :uintptr))
  (output-decode-data (:pointer (:pointer :float))))
(defun decode (length
               phoneme-size
               f0
               phoneme-vector
               speaker-id)
  (let ((real-phoneme-size (* length phoneme-size)))
    (cffi:with-foreign-objects ((c-f0 :float length)
                                (c-phoneme :float real-phoneme-size)
                                (output-decode-data-length :float)
                                (output-decode-data '(:pointer :float)))
      (loop for i from 0 below length
            do (setf (cffi:mem-aref c-f0 :float i) (aref f0 i)
                     (cffi:mem-aref c-phoneme :float i) (aref c-phoneme i)))
      (loop for i from (- length 1) below real-phoneme-size
            do (setf (cffi:mem-aref c-phoneme :float i) (aref c-phoneme i)))
      (let ((result-status
              (get-result-from-code
               (vv-decode
                length
                phoneme-size
                c-f0
                c-phoneme
                output-decode-data-length
                output-decode-data))))
        (if (eq result-status :voicevox-result-ok)
            (let ((output-decode-data-length-unref (cffi:mem-ref output-decode-data-length :uintptr))
                  (output-decode-data-lisp-array
                    (make-array-from-pointer output-decode-data
                                             output-decode-data-length-unref
                                             '(:pointer :float)
                                             :float)))
              (list :result-status result-status
                    :decode-data-length output-decode-data-length-unref
                    :decode-data output-decode-data-lisp-array))
            (list :result-status result-status))))))

(cffi:defcfun ("voicevox_synthesis" vv-synthesis) :int
  (audio-query-json (:pointer :char))
  (speaker-id :uint32)
  (options (:struct voicevox-synthesis-options))
  (output-wav-length (:pointer :uintptr))
  (output-wav (:pointer (:pointer :uint8))))
(defun synthesis (&key audio-query-json speaker-id enable-interrogative-upspeak)
  (cffi:with-foreign-objects ((output-wav-length :uintptr)
                              (output-wav '(:pointer :uint8))
                              (options-synthesis '(:struct voicevox-synthesis-options)))
    (setf
     (cffi:foreign-slot-value options-synthesis '(:struct voicevox-synthesis-options) 'enable-interrogative-upspeak)
     (if enable-interrogative-upspeak 1 0))
    (cffi:with-foreign-strings ((c-audio-query-json audio-query-json))
      (let ((result-status
              (get-result-from-code
               (vv-synthesis
                c-audio-query-json
                speaker-id
                (cffi:mem-ref options-synthesis '(:struct voicevox-synthesis-options))
                output-wav-length
                output-wav))))
        (if (eq result-status :voicevox-result-ok)
            (let* ((output-wav-length-unref (cffi:mem-ref output-wav-length :uintptr))
                   (output-wav-lisp-array (make-array-from-pointer output-wav
                                                                   output-wav-length-unref
                                                                   '(:pointer :uint8)
                                                                   :uint8)))
              (list :result-status result-status
                    :wav-length output-wav-length-unref
                    :wav-bytes output-wav-lisp-array ))
            (list :result-status result-status))))))


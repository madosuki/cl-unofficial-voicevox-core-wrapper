(in-package :cl-user)
(defpackage unofficial-voicevox-core-wrapper-cl
  (:use :cl :cffi)
  (:nicknames :unofficial-vv-core-wrapper)
  (:import-from
   :unofficial-voicevox-core-wrapper-cl.types
   :uint32
   :uint16
   :voicevox-result-code-type
   :voicevox-acceleration-mode-type)
  (:export
   :voicevox-result-code-type
   :voicevox-acceleration-mode-type
   #:initialize
   #:generate-wav
   #:generate-audio-query
   #:load-library
   #:load-model
   #:get-version
   #:is-gpu-mode
   #:load-model
   #:is-model-loaded
   #:finalize
   #:get-supported-version
   #:get-metas-json))
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

(declaim (inline get-result-from-code))
(defun get-result-from-code (code)
  (cffi:foreign-enum-keyword 'voicevox-result-code code))

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
  (let ((options-ini (cffi:foreign-alloc '(:struct voicevox-initialize-options)))
        (open-jtalk-dict-dir-path-c (cffi:foreign-string-alloc open-jtalk-dict-dir
                                                               :encoding :UTF-8)))
    (setf (cffi:foreign-slot-value options-ini '(:struct voicevox-initialize-options) 'acceleration_mode) acceleration-mode
          (cffi:foreign-slot-value options-ini '(:struct voicevox-initialize-options) 'cpu_num_threads) cpu-num-threads
          (cffi:foreign-slot-value options-ini '(:struct voicevox-initialize-options) 'load_all_models) load-all-models
          (cffi:foreign-slot-value options-ini '(:struct voicevox-initialize-options) 'open_jtalk_dict_dir) open-jtalk-dict-dir-path-c)
    (let ((result-status
            (get-result-from-code (vv-initialize (cffi:mem-aref options-ini '(:struct voicevox-initialize-options))))))
      (free-for-foreign
       options-ini)
      (cffi:foreign-string-free open-jtalk-dict-dir-path-c)
      result-status)))

(cffi:defcfun ("voicevox_is_gpu_mode" is-gpu-mode) :bool)
(cffi:defcfun ("voicevox_get_version" get-version) :string)
(cffi:defcfun ("voicevox_load_model" load-model) :int
  (speaker-id :uint32))
(cffi:defcfun ("voicevox_is_model_loaded" is-model-loaded) :bool
  (speaker-id :uint32))
(cffi:defcfun ("voicevox_finalize" finalize) :void)
(cffi:defcfun ("voicevox_get_metas_json" get-metas-json) :string)
(cffi:defcfun ("voicevox_get_supported_device_json" get-supported-device-json) :string)


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

(defun make-array-from-pointer (target length pointer-type value-type)
  (let ((result (make-array length)))
    (loop for i from 0 below length
          do
             (let ((tmp (cffi:mem-aref (cffi:mem-aref target pointer-type 0)
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
                generate-audio-query))
(defun generate-audio-query (&key text kana speaker-id)
  (let ((audio-query-options (cffi:foreign-alloc '(:struct voicevox-audio-query-options)))
        (out-audio-query-json (cffi:foreign-alloc '(:pointer (:pointer :char))))
        (text-c (cffi:foreign-string-alloc text))
        (result nil))
    (setf (cffi:foreign-slot-value audio-query-options '(:struct voicevox-audio-query-options) 'kana) (if kana 1 0))
    (let ((result-status (get-result-from-code
                         (vv-audio-query
                          text-c
                          speaker-id
                          (cffi:mem-ref audio-query-options '(:struct voicevox-audio-query-options))
                          out-audio-query-json))))
      (if (equal result-status :voicevox-result-ok)
          (setq result (jojo:parse (cffi:foreign-string-to-lisp (cffi:mem-aref out-audio-query-json '(:pointer (:pointer :char))))))
          (setq result result-status))
      (free-for-foreign
       audio-query-options
       out-audio-query-json)
      (cffi:foreign-string-free text-c)
      (list :result-status result-status :audio-query result))))

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
        (result-status :voicevox-result-ok))
    (setf
     (cffi:foreign-slot-value options-tts '(:struct voicevox-tts-options) 'kana) kana
     (cffi:foreign-slot-value options-tts '(:struct voicevox-tts-options) 'enable-interrogative-upspeak) enable-interrogative-upspeak)
    (setq result-status
          (get-result-from-code
           (vv-tts
            text-c
            speaker-id
            (cffi:mem-ref options-tts '(:struct voicevox-tts-options))
            wav-length
            out-wav-bytes
            )))
    (let* ((wav-length-unrefed (cffi:mem-ref wav-length :uint))
           (wav-bytes-array (make-array-from-pointer
                             out-wav-bytes
                             wav-length-unrefed
                             '(:pointer (:pointer :uint8))
                             :uint8)))
      (free-for-foreign
       options-tts
       wav-length
       out-wav-bytes)
      (cffi:foreign-string-free text-c)
      (list :result-status result-status :wav-length wav-length-unrefed :wav-bytes wav-bytes-array))))


(defun load-library (path)
  (cffi:load-foreign-library path))


;; (defcfun ("voicevox_predict_duration" vv-predict-duration) :int
;;   (length :uintptr)
;;   (phoneme-vector (:pointer :int64))
;;   (speaker-id :uint32)
;;   (output-predict-durarion-data-length (:pointer :uintptr))
;;   (output-predict-durarion-data (:pointer (:pointer :float))))
;; (defun predict-duration (length phoneme-vector speaker-id)
;;   (cffi:with-foreign-objects ((arr :int64 length))
;;     (loop for i from 0 below length
;;           do (setf (cffi:mem-aref arr :int64 i) (aref phoneme-vector i)))
;;     (let ((output-predict-durarion-data-length (cffi:foreign-alloc :uintptr))
;;           (output-predict-durarion-data (cffi:foreign-alloc '(:pointer (:pointer :float)))))
;;       (let ((result-status (get-result-from-code (vv-predict-duration
;;                                                   length
;;                                                   arr
;;                                                   output-predict-durarion-data-length
;;                                                   output-predict-durarion-data))))
;;         (if (eq result-status :voicevox-result-ok)
;;             (list :result-status result-status
;;                   :output-predicet-duration-data-length (cffi:mem-ref output-predict-durarion-data-length :uintptr)
;;                   )
;;             (list :result-status result-status))))))

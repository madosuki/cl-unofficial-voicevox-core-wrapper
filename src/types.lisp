(in-package :cl-user)
(defpackage unofficial-voicevox-core-wrapper-cl.types
  (:use :cl :cffi)
  (:export :uint16
           :uint32
           :voicevox-initialize-options
           :voicevox-audio-query-options
           :voicevox-tts-options
           :voicevox-result-code
           :voicevox-result-code-type
           :voicevox-acceleration-mode
           :voicevox-acceleration-mode-type))
(in-package :unofficial-voicevox-core-wrapper-cl.types)

(deftype uint16 () '(integer 0 65535))
(deftype uint32 () '(integer 0 4294967295))

(cffi:defcenum voicevox-acceleration-mode
  (:voicevox-acceleration-mode-auto 0)
  (:voicevox-acceleration-mode-cpu 1)
  (:voicevox-acceleration-mode-gpu 2))
(deftype voicevox-acceleration-mode-type ()
  '(member :voicevox-acceleration-mode-auto :voicevox-acceleration-mode-cpu :voicevox-acceleration-mode-gpu))

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
(deftype voicevox-result-code-type ()
  '(member
    :voicevox-result-ok
    :voicevox-result-not-loaded-openjtalk-dict-error
    :voicevox-result-load-model-error
    :voicevox-result-get-supported-device-error
    :voicevox-result-gpu-support-error
    :voicevox-result-load-metas-error
    :voicevox-result-uninitialized-status-error
    :voicevox-result-invalid-speaker-id-error
    :voicevox-result-invalid-model-index-error
    :voicevox-result-inference-error
    :voicevox-result-extract-full-context-label-error
    :voicevox-result-invalid-utf8-input-error
    :voicevox-result-parse-kana-error
    :voicevox-result-invalid-audio-query-error))

(cffi:defcstruct voicevox-initialize-options
  (acceleration_mode voicevox-acceleration-mode)
  (cpu_num_threads :uint16)
  (load_all_models :bool)
  (open_jtalk_dict_dir (:pointer :char)))

(cffi:defcstruct voicevox-audio-query-options
  (kana :int))

(cffi:defcstruct voicevox-tts-options
  (kana :bool)
  (enable-interrogative-upspeak :bool))

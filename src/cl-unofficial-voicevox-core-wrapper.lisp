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
   #:close-library
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

(cffi:defcenum voicevox-acceleration-mode-enum
  (:voicevox-acceleration-mode-auto 0)
  (:voicevox-acceleration-mode-cpu 1)
  (:voicevox-acceleration-mode-gpu 2))


(cffi:defcenum voicevox-result-code
  (:voicevox-result-ok 0)
  (:voicevox-result-not-loaded-openjtalk-dict-error 1)
  (:voicevox-result-load-model-error 2)
  (:voicevox-result-get-supported-devices-error 3)
  (:voicevox-result-gpu-support-error 4)
  (:voicevox-result-init-inference-runtime-error 29)
  (:voicevox-result-style-not-found-error 6)
  (:voicevox-result-model-not-found-error 7)
  (:voicevox-result-run-model-error 8)
  (:voicevox-result-analyze-text-error 11)
  (:voicevox-result-invalid-utf8-input-error 12)
  (:voicevox-result-parse-kana-error 13)
  (:voicevox-result-invalid-audio-query-error 14)
  (:voicevox-result-invalid-accent-phrase-error 15)
  (:voicevox-result-open-zip-file-error 16)
  (:voicevox-result-read-zip-entry-error 17)
  (:voicevox-result-invalid-model-header-error 28)
  (:voicevox-result-model-already-loaded-error 18)
  (:voicevox-result-style-already-loaded-error 26)
  (:voicevox-result-invalid-model-data-error 27)
  (:voicevox-result-load-user-dict-error 20)
  (:voicevox-result-save-user-dict-error 21)
  (:voicevox-result-user-dict-word-not-found-error 22)
  (:voicevox-result-use-user-dict-error 23)
  (:voicevox-result-invalid-user-dict-word-error 24)
  (:voicevox-result-invalid-uuid-error 25))

(cffi:defcenum voicevox-user-dict-word-type
  (:voicevox-user-dict-word-type-proper-noun 0)
  (:voicevox-user-dict-word-type-common-noun 1)
  (:voicevox-user-dict-word-type-verb 2)
  (:voicevox-user-dict-word-type-adjective 3)
  (:voicevox-user-dict-word-type-suffix 4))

(cffi:defcstruct open-jtalk-rc)
(cffi:defcstruct voicevox-onnxruntime)
(cffi:defcstruct voicevox-user-dict)
(cffi:defcstruct voicevox-voice-model-file)

(cffi:defcstruct voicevox-load-onnexruntime-options
  (filename (:pointer :char)))

(cffi:defcstruct voicevox-initialize-optons
  (acceleration_mode voicevox-acceleration-mode-enum)
  (cpu_num_threads :uint16))

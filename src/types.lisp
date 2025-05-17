(in-package :cl-user)
(defpackage cl-unofficial-voicevox-core-wrapper.types
  (:use :cl :cffi)
  (:export :uint16
           :uint32
           :voicevox-result-code-type
           :voicevox-acceleration-mode-type))
(in-package :cl-unofficial-voicevox-core-wrapper.types)

(deftype uint8 () '(unsigned-byte 8))
(deftype uint16 () '(unsigned-byte 16))
(deftype uint32 () '(unsigned-byte 32))

(deftype voicevox-acceleration-mode-type ()
  '(member :voicevox-acceleration-mode-auto :voicevox-acceleration-mode-cpu :voicevox-acceleration-mode-gpu))


(deftype voicevox-result-code-type ()
  '(member
    :voicevox-result-ok
    :voicevox-result-not-loaded-openjtalk-dict-error
    :voicevox-result-get-supported-devices-error
    :voicevox-result-gpu-support-error
    :voicevox-result-init-inference-runtime-error
    :voicevox-result-style-not-found-error
    :voicevox-result-model-not-found-error
    :voicevox-result-run-model-error
    :voicevox-result-analyze-text-error
    :voicevox-result-invalid-utf8-input-error
    :voicevox-result-parse-kana-error
    :voicevox-result-invalid-audio-query-error
    :voicevox-result-invalid-accent-phrase-error
    :voicevox-result-open-zip-file-error
    :voicevox-result-read-zip-entry-error
    :voicevox-result-invalid-model-header-error
    :voicevox-result-model-already-loaded-error
    :voicevox-result-style-already-loaded-error
    :voicevox-result-invalid-model-data-error
    :voicevox-result-load-user-dict-error
    :voicevox-result-save-user-dict-error
    :voicevox-result-user-dict-word-not-found-error
    :voicevox-result-use-user-dict-error
    :voicevox-result-invalid-user-dict-word-error
    :voicevox-result-inavlid-uuid-error))



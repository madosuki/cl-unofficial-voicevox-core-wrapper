(in-package :cl-user)
(defpackage cl-unofficial-voicevox-core-wrapper.types
  (:use :cl :cffi)
  (:export :uint16
           :uint32
           :voicevox-result-code-type
           :voicevox-acceleration-mode-type))
(in-package :cl-unofficial-voicevox-core-wrapper.types)

(deftype uint16 () '(unsigned-byte 16))
(deftype uint32 () '(unsigned-byte 32))

(deftype voicevox-acceleration-mode-type ()
  '(member :voicevox-acceleration-mode-auto :voicevox-acceleration-mode-cpu :voicevox-acceleration-mode-gpu))


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



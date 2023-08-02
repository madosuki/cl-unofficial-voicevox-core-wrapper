(in-package :cl-user)
(defpackage cl-unofficial-voicevox-core-wrapper.types
  (:use :cl :cffi)
  (:export :uint16
           :uint32
           :voicevox-result-code-type
           :voicevox-acceleration-mode-type
           :voicevox-user-dict-word-type-for-enum
           :voicevox-style-id-type))
(in-package :cl-unofficial-voicevox-core-wrapper.types)

(deftype uint16 () '(unsigned-byte 16))
(deftype uint32 () '(unsigned-byte 32))
(deftype voicevox-style-id-type () '(unsigned-byte 32))

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
    :voicevox-result-invalid-style-id-error
    :voicevox-result-invalid-model-index-error
    :voicevox-result-inference-error
    :voicevox-result-extract-full-context-label-error
    :voicevox-result-invalid-utf8-input-error
    :voicevox-result-parse-kana-error
    :voicevox-result-invalid-audio-query-error
    :voicevox-result-invalid-accent-phrase-error
    :voicevox-open-file-error
    :voicevox-vvm-model-read-error
    :voicevox-already-loaded-model-error
    :voicevox-unloaded-model-error
    :voicevox-load-user-dict-error
    :voicevox-save-user-dict-error
    :voicevox-unknown-user-dict-word-error
    :voicevox-use-user-dict-error
    :voicevox-invalid-user-dict-word-error
    :voicevox-result-invalid-uuid-error))


(deftype voicevox-user-dict-word-type-for-enum ()
  '(member
   :voicevox-user-dict-word-type-proper-noun
   :voicevox-user-dict-word-type-common-noun
   :voicevox-user-dict-word-type-verb
   :voicevox-user-dict-word-type-adjective
   :voicevox-user-dict-word-type-suffix))

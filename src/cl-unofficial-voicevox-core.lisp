(defpackage :cl-unofficial-voicevox-core-wrapper
  (:use :cl)
  (:export
   ;; library management functions
   #:load-library
   #:close-library

   ;; general
   #:get-version
   #:get-result-from-code
   #:error-result-to-message
   #:get-onnxruntime-lib-recommend-versioned-filename
   #:get-onnxruntime-lib-recommend-unversioned-filename

   ;; ONNX Runtime
   #:onnxruntime-class
   #:onnxruntime-init
   #:onnxruntime-get

   ;; Open JTalk
   #:open-jtalk-rc-class
   #:open-jtalk-rc-init
   #:open-jtalk-rc-delete
   #:open-jtalk-rc-close
   #:open-jtalk-rc-use-user-dict
   #:open-jtalk-rc-analyze

   ;; VoiceVox
   #:voicevox-class
   #:voicevox-synthesizer-init
   #:voicevox-synthesizer-delete
   #:voicevox-synthesizer-load-voice-model
   #:voicevox-synthesizer-unload-voice-model
   #:voicevox-synthesizer-is-gpu-mode
   #:voicevox-synthesizer-is-loaded-voice-model
   #:voicevox-synthesizer-create-metas-json
   #:voicevox-synthesizer-create-audio-query
   #:voicevox-synthesizer-create-audio-query-from-kana
   #:voicevox-synthesizer-create-accent-phrases
   #:voicevox-synthesizer-create-accent-phrases-from-kana
   #:voicevox-synthesizer-replace-mora-data
   #:voicevox-synthesizer-replace-phoneme-length
   #:voicevox-synthesizer-replace-mora-pitch
   #:voicevox-synthesizer-synthesis
   #:voicevox-synthesizer-tts
   #:voicevox-synthesizer-tts-from-kana
   #:voicevox-synthesizer-create-sing-frame-audio-query
   #:voicevox-synthesizer-create-sing-frame-f0
   #:voicevox-synthesizer-create-sing-frame-volume
   #:voicevox-synthesizer-frame-synthesis

   ;; Voice model
   #:voice-model-file-class
   #:voice-model-file-open
   #:voice-model-file-id
   #:voice-model-file-create-metas-json
   #:voice-model-file-delete
   #:voice-model-file-close

   ;; User dictionary
   #:user-dict-class
   #:user-dict-new
   #:user-dict-load
   #:user-dict-add-word
   #:user-dict-update-word
   #:user-dict-remove-word
   #:user-dict-to-json
   #:user-dict-import
   #:user-dict-save
   #:user-dict-delete))
(in-package :cl-unofficial-voicevox-core-wrapper)

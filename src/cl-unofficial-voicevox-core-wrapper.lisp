(in-package :cl-user)
(defpackage cl-unofficial-voicevox-core-wrapper
  (:use :cl :cffi)
  (:nicknames :unofficial-vv-core-wrapper)
  (:import-from
   :cl-unofficial-voicevox-core-wrapper.types
   :uint8
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

(cffi:defcenum voicevox-acceleration-mode
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

(cffi:defcenum voicevox-user-dict-word
  (:voicevox-user-dict-word-type-proper-noun 0)
  (:voicevox-user-dict-word-type-common-noun 1)
  (:voicevox-user-dict-word-type-verb 2)
  (:voicevox-user-dict-word-type-adjective 3)
  (:voicevox-user-dict-word-type-suffix 4))

(cffi:defcstruct open-jtalk-rc)
(cffi:defcstruct voicevox-onnxruntime)
(cffi:defcstruct voicevox-synthesizer)
(cffi:defcstruct voicevox-user-dict)
(cffi:defcstruct voicevox-voice-model-file)

(cffi:defcstruct voicevox-load-onnexruntime-options
  (filename (:pointer :char)))

(cffi:defcstruct voicevox-initialize-optons
  (acceleration_mode voicevox-acceleration-mode-enum)
  (cpu-num-threads :uint16))

(cffi:defctype voicevox-model-id (:pointer (:array :uint8 16)))
(cffi:defctype voicevox-style-id :uint32)

(cffi:defcstruct voicevox-synthesis-options
  (enable-interrogative-upspeak :int))

(cffi:defcstruct voicevox-tts-options
  (enable-interrogative-upspeak :int))

(cffi:defcstruct voicevox-user-dict-word
  (surface (:pointer :char))
  (pronunciation (:pointer :char))
  (accent_type (:pointer :uint))
  (word-type voicevox-user-dict-word)
  (priority :uint32))

;; if defined VOICEVOX_LOAD_ONNXRUNTIME
(cffi:defcfun ("voicevox_get_onnxruntime_lib_versioned_filename" vv-get-onnxruntime-lib-versioned-filename)
    (:pointer :char))
(cffi:defcfun ("voicevox_get_onnxruntime_lib_unversioned_filename" vv-get-onnxruntime-lib-unversioned-filename)
    (:pointer :char))
;; endif

(cffi:defcfun ("voicevox_make_default_load_onnxruntime_options" vv-make-default-load-onnxruntime-options)
    (:pointer (:struct voicevox-load-onnexruntime-options)))
(cffi:defcfun ("voicevox_onnxruntime_get" vv-onnxruntime-get) (:pointer (:struct voicevox-onnxruntime)))

;; if defined VOICEVOX_LOAD_ONNXRUNTIME
(cffi:defcfun ("voicevox_onnxruntime_load_once" vv-onnxruntime-load-once) :int
  (options (:struct voicevox-load-onnexruntime-options))
  (out-onnxruntime (:pointer (:pointer (:struct voicevox-onnxruntime)))))
;; endif

;; if defined VOICEVOX_LINK_ONNXRUNTIME
(cffi:defcfun ("voicevox_onnxruntime_init_once" vv-onnxruntime-init-once) :int
  (out-onnxruntime (:pointer (:pointer (:struct voicevox-onnxruntime)))))
;; endif

(cffi:defcfun ("voievox_open_jtalk_rc_new" vv-open-jtalk-rc-new) :int
  (open-jtalk-dic-dir (:pointer :char))
  (out-open-jtalk (:pointer (:pointer (:struct open-jtalk-rc)))))

(cffi:defcfun ("voicevox_open_jtalk_rc_use_user_dict" vv-open-jtalk-rc-use-user-dict) :int
  (open-jtalk (:pointer (:struct open-jtalk-rc)))
  (user-dict (:pointer (:struct voicevox-user-dict))))

(cffi:defcfun ("voicevox_open_jtalk_rc_analyze" vv-open-jtalk-rc-analyze) :int
  (open-jtalk (:pointer (:struct open-jtalk-rc)))
  (text (:pointer :char))
  (output-accent-phrase-json (:pointer (:pointer :char))))

(cffi:defcfun ("voicevox_open_jtalk_rc_delete" vv-open-jtalk-rc-delete) :void
  (open-jtalk (:pointer (:struct open-jtalk-rc))))

(cffi:defcfun ("voievox_make_default_initialize_options" vv-make-default-initialize-options)
    (:struct voicevox-initialize-optons))

(cffi:defcfun ("voicevox_get_version" vv-get-version) (:pointer (:char)))

(cffi:defcfun ("voicevox_audio_query_create_from_accent_phrase" vv-audio-query-create-from-accent-phrase) :int
  (accent-phrase-json (:pointer :char))
  (output-audio-query-json (:pointer (:pointer :char))))

(cffi:defcfun ("voicevox_voice_model_file_open" vv-voice-model-file-open) :int
  (path (:pointer :char))
  (out-model (:pointer (:pointer (:struct voicevox-model-file)))))

(cffi:defcfun ("voicevox_voice_model_file_id" vv-voice-model-file-id) :void
  (model (:pointer (:struct voicevox-model-file)))
  (output-voice-model-id (:pointer (:array :uint8 16))))

(cffi:defcfun ("voicvox_voice_model_file_create_metas_json" vv-voice-model-file-create-metas-json) (:pointer :char)
  (model (:pointer (:struct voicevox-model-file))))

(cffi:defcfun ("voicevox_voice_model_file_delete" vv-voice-model-file-delete) :void)

(cffi:defcfun ("voicvox_synthesizer_new" vv-synthesizer-new) :int
  (synthesizer (:pointer (:struct synthesizer)))
  (model (:pointer (:struct voicevox-voice-model-file))))

(cffi:defcfun ("voicevox_synthesizer_unload_voice_model" vv-synthesizer-unload-voice-model) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (model-id voicevox-model-id))

(cffi:defcfun ("voicevox_synthesizer_get_onnxruntime" vv-synthesizer-get-onnxruntime) (:pointer (:struct voicevox-onnxruntime))
  (synthesizer (:pointer (:struct voicevox-synthesizer))))

(cffi:defcfun ("voicevox_synthesizer_is_gpu_mode" vv-synthesizer-is-gpu-mode) :int)

(cffi:defcfun ("voicevox_synthesizer_is_loaded_voice_model" vv-synthesizer-is-loaded-voice-model) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (model-id voicevox-model-id))

(cffi:defcfun ("voicevox_synthesizer_create_metas_json" vv-synthesizer-create-mtas-json) (:pointer :char)
  (synthesizer (:pointer (:struct voicevox-synthesizer))))

(cffi:defcfun ("voicevox_onnxruntime_create_supported_devices_json" vv-onnxruntime-create-supported-devices-json) :int
  (onnxruntim (:pointer (:struct voicevox-onnxruntime)))
  (output-supported-devices-json (:pointer (:pointer :char))))

(cffi:defcfun ("voicevox_synthesizer_create_audio_query_from_kana" vv-synthesizer-create-audio-query-from-kana) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (kana (:pointer :char))
  (style-id voicevox-style-id)
  (output-audio-query-json (:pointer (:pointer :char))))

(cffi:defcfun ("voicevox_synthesizer_create_audio_query" vv-synthesizer-create-audio-query) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (text (:pointer :char))
  (style-id voicevox-style-id)
  (output-qudio-query-json (:pointer (:pointer char))))

(cffi:defcfun ("voicevox_synthesizer_create_accent_phrases_from_kana" vv-synthesizer-create-accent-phrases-from-kana) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (kana (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrase-json (:pointer (:pointer :char))))

(cffi:defcfun ("voicevox_synthesizer_create_accent_phrases" vv-synthesizer-create-accent-phrases) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (text (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrase-json (:pointer (:pointer :char))))

(cffi:defcfun ("voicevox_synthesizer_replace_mora_data" vv-synthesizer-replace-mora-data) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (accent-phrase-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrase-json (:pointer (:pointer :char))))

(cffi:defcfun ("voicevox_synthesizer_replace_phoneme_length" vv-synthesizer-replace-phoneme-length) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (accent-phrase-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrase-json (:pointer (:pointer :char))))

(cffi:defcfun ("voicevox_synthesizer_replace_mora_pitch" vv-synthesizer-replace-mora-pitch) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (accent-phrase-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrase-json (:pointer (:pointer :char))))

(cffi:defcfun ("voicevox_synthesizer_synthesis" vv-synthesizer-synthesis) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (audio-query-json (:pointer :char))
  (style-id voicevox-style-id)
  (options (:struct voicevox-synthesis-options))
  (output-wav-length (:pointer :uintptr))
  (output-wav (:pointer (:pointer :uint8))))

(cffi:defcfun ("voicevox_make_default_tts_options" vv-make-default-tts-options) (:struct voicevox-tts-options))

(cffi:defcfun ("voicevox_synthesizer_tts_from_kana" vv-synthesizer-tts-from-kana) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (kana (:pointer :char))
  (style-id voicevox-style-id)
  (options (:struct voicevox-tts-options))
  (output-wav-length (:pointer :uintptr))
  (output-wav (:pointer (:pointer :uint8))))

(cffi:defcfun ("voicevox_synthesizer_tts" vv-synthesizer-tts) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (text (:pointer :char))
  (style-id voicevox-style-id)
  (options (:struct voicevox-tts-options))
  (output-wav-length (:pointer :uintptr))
  (output-wav (:pointer (:pointer :uint8))))

(cffi:defcfun ("voicevox_json_free" vv-json-free) :void
  (json (:pointer :char)))

(cffi:defcfun ("voicevox_wav_free" vv-wav-free) :void
  (wav (:pointer :uint8)))

(cffi:defcfun ("voicevox_error_result_to_message" vv-error-result-to-message) (:pointer :char)
  (result-code :int))

(cffi:defcfun ("voicevox_user_dict_word_make" vv-user-dict-word-make) (:struct voicevox-user-dict-word)
  (surface (:pointer :char))
  (pronunciation (:pointer :char))
  (accent-type :uintptr))

(cffi:defcfun ("voicevox_user_dict_new" vv-user-dict-new) (:struct voicevox-user-dict))

(cffi:defcfun ("voicevox_user_dict_load" vv-user-dict-load) :int
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (dict-path (:pointer :char)))

(cffi:defcfun ("voicvox_user_dict_add_word" vv-user-dict-add-word) :int
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (word (:pointer (:struct voicevox-user-dict-word)))
  (output-word-uuid (:pointer (:array :uint8 16))))

(cffi:defcfun ("voicevox_user_dict_update_word" vv-user-dict-update-word) :int
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (word-uuid (:pointer (:array :uint8 16)))
  (word (:pointer (:struct voicevox-user-dict-word))))

(cffi:defcfun ("voicevox_user_dict_remove_word" vv-user-dict-remove-word) :int
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (word-uuid (:pointer (:array :uint8 16))))

(cffi:defcfun ("voicevox_user_dict_to_json" vv-user-dict-to-json) :int
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (output-json (:pointer (:pointer :char))))

(cffi:defcfun ("voicevox_user_dict_import" vv-user-dict-import) :int
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (other-dict (:pointer (:struct voicevox-user-dict))))

(cffi:defcfun ("voicevox_user_dict_save" vv-user-dict-save) :int
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (path (:pointer :char)))

(cffi:defcfun ("voicevox_user_dict_delete" vv-user-dict-delete) :void
  (user-dict (:pointer (:struct voicevox-user-dict))))


(in-package :cl-user)
(defpackage cl-unofficial-voicevox-core-wrapper.binding
  (:use :cl)
  (:import-from
   :cl-unofficial-voicevox-core-wrapper.types
   :uint8
   :uint16
   :uint32
   :voicevox-result-code-type
   :voicevox-acceleration-mode-type
   :voicevox-user-dict-word-type-type
   :voicevox-on-existing-voice-model-id-type)
  (:export
   :uint8
   :uint16
   :uint32
   :voicevox-result-code-type
   :voicevox-acceleration-mode-type
   :voicevox-user-dict-word-type-type
   :voicevox-acceleration-mode
   :voicevox-result-code
   :voicevox-user-dict-word-type
   :voicevox-load-onnxruntime-options
   :voicevox-initialize-options
   :voicevox-load-voice-model-options
   :voicevox-synthesis-options
   :voicevox-tts-options
   :voicevox-user-dict-word
   :voicevox-style-id
   :voicevox-voice-model-id
   :get-result-from-code
   :error-result-to-message
   :get-onnxruntime-lib-recommend-versioned-filename
   :get-onnxruntime-lib-recommend-unversioned-filename
   :vv-get-onnxruntime-lib-min-required-minor-version
   :vv-get-onnxruntime-lib-max-supported-minor-version
   :onnxruntime-class
   :onnxruntime-init
   :onnxruntime-get
   :onnxruntime-ptr
   :open-jtalk-rc-class
   :open-jtalk-rc-init
   :open-jtalk-rc-delete
   :open-jtalk-rc-close
   :open-jtalk-rc-use-user-dict
   :open-jtalk-rc-analyze
   :open-jtalk-rc-ptr
   :voicevox-class
   :voicevox-synthesizer-init
   :voicevox-synthesizer-delete
   :vv-make-default-load-voice-model-options
   :voicevox-synthesizer-load-voice-model
   :voicevox-synthesizer-unload-voice-model
   :voicevox-synthesizer-is-gpu-mode
   :voicevox-synthesizer-is-loaded-voice-model
   :voicevox-synthesizer-create-metas-json
   :voicevox-synthesizer-create-audio-query
   :voicevox-synthesizer-create-audio-query-from-kana
   :voicevox-synthesizer-create-accent-phrases
   :voicevox-synthesizer-create-accent-phrases-from-kana
   :voicevox-synthesizer-replace-mora-data
   :voicevox-synthesizer-replace-phoneme-length
   :voicevox-synthesizer-replace-mora-pitch
   :voicevox-synthesizer-synthesis
   :voicevox-synthesizer-tts
   :voicevox-synthesizer-tts-from-kana
   :voicevox-synthesizer-create-sing-frame-audio-query
   :voicevox-synthesizer-create-sing-frame-f0
   :voicevox-synthesizer-create-sing-frame-volume
   :voicevox-synthesizer-frame-synthesis
   :voice-model-file-class
   :voice-model-file-open
   :voice-model-file-id
   :voice-model-file-create-metas-json
   :voice-model-file-delete
   :voice-model-file-close
   :voice-model-file-ptr
   :user-dict-class
   :user-dict-new
   :user-dict-load
   :user-dict-add-word
   :user-dict-update-word
   :user-dict-remove-word
   :user-dict-to-json
   :user-dict-import
   :user-dict-save
   :user-dict-delete
   :user-dict-ptr
   :vv-get-onnxruntime-lib-versioned-filename
   :vv-get-onnxruntime-lib-unversioned-filename
   :vv-make-default-load-onnxruntime-options
   :vv-onnxruntime-get
   :vv-onnxruntime-load-once
   :vv-onnxruntime-init-once
   :vv-open-jtalk-rc-new
   :vv-open-jtalk-rc-use-user-dict
   :vv-open-jtalk-rc-analyze
   :vv-open-jtalk-rc-delete
   :vv-make-default-initialize-options
   :vv-get-version
   :vv-audio-query-create-from-accent-phrases
   :vv-audio-query-validate
   :vv-accent-phrase-validate
   :vv-mora-validate
   :vv-score-validate
   :vv-note-validate
   :vv-frame-audio-query-validate
   :vv-frame-phoneme-validate
   :vv-ensure-compatible
   :vv-voice-model-file-open
   :vv-voice-model-file-id
   :vv-voice-model-file-create-metas-json
   :vv-voice-model-file-delete
   :vv-synthesizer-new
   :vv-synthesizer-delete
   :vv-synthesizer-load-voice-model
   :vv-synthesizer-unload-voice-model
   :vv-synthesizer-get-onnxruntime
   :vv-synthesizer-is-gpu-mode
   :vv-synthesizer-is-loaded-voice-model
   :vv-synthesizer-create-metas-json
   :vv-onnxruntime-create-supported-devices-json
   :vv-synthesizer-create-audio-query-from-kana
   :vv-synthesizer-create-audio-query
   :vv-synthesizer-create-accent-phrases-from-kana
   :vv-synthesizer-create-accent-phrases
   :vv-synthesizer-replace-mora-data
   :vv-synthesizer-replace-phoneme-length
   :vv-synthesizer-replace-mora-pitch
   :vv-make-default-synthesis-options
   :vv-synthesizer-synthesis
   :vv-make-default-tts-options
   :vv-synthesizer-tts-from-kana
   :vv-synthesizer-tts
   :vv-synthesizer-create-sing-frame-audio-query
   :vv-synthesizer-create-sing-frame-f0
   :vv-synthesizer-create-sing-frame-volume
   :vv-synthesizer-frame-synthesis
   :vv-json-free
   :vv-wav-free
   :vv-error-result-to-message
   :vv-user-dict-word-make
   :vv-user-dict-new
   :vv-user-dict-load
   :vv-user-dict-add-word
   :vv-user-dict-update-word
   :vv-user-dict-remove-word
   :vv-user-dict-to-json
   :vv-user-dict-import
   :vv-user-dict-save
   :vv-user-dict-delete))
(in-package :cl-unofficial-voicevox-core-wrapper.binding)

(cffi:defcenum voicevox-acceleration-mode
  (:voicevox-acceleration-mode-auto 0)
  (:voicevox-acceleration-mode-cpu 1)
  (:voicevox-acceleration-mode-gpu 2))

(cffi:defcenum voicevox-on-existing-voice-model-id
  (:voicevox-on-existing-voice-model-id-error 0)
  (:voicevox-on-existing-voice-model-id-reload 1)
  (:voicevox-on-existing-voice-model-id-skip 2))

(cffi:defcenum voicevox-result-code
    (:voicevox-result-ok 0)
  (:voicevox-result-not-loaded-openjtalk-dict-error 1)
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
  (:voicevox-result-invalid-model-format-error 28)
  (:voicevox-result-model-already-loaded-error 18)
  (:voicevox-result-style-already-loaded-error 26)
  (:voicevox-result-invalid-model-data-error 27)
  (:voicevox-result-load-user-dict-error 20)
  (:voicevox-result-save-user-dict-error 21)
  (:voicevox-result-user-dict-word-not-found-error 22)
  (:voicevox-result-use-user-dict-error 23)
  (:voicevox-result-invalid-user-dict-word-error 24)
  (:voicevox-result-invalid-uuid-error 25)
  (:voicevox-result-invalid-mora-error 30)
  (:voicevox-result-invalid-score-error 31)
  (:voicevox-result-invalid-note-error 32)
  (:voicevox-result-invalid-frame-audio-query-error 33)
  (:voicevox-result-invalid-frame-phoneme-error 34)
  (:voicevox-result-incompatible-queries-error 35))

(cffi:defcenum voicevox-user-dict-word-type
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

(cffi:defcstruct voicevox-load-onnxruntime-options
  (filename (:pointer :char)))

(cffi:defcstruct voicevox-initialize-options
  (acceleration-mode voicevox-acceleration-mode)
  (cpu-num-threads :uint16))

(cffi:defcstruct voicevox-load-voice-model-options
  (on-existing voicevox-on-existing-voice-model-id))

(cffi:defctype voicevox-voice-model-id (:pointer (:array :uint8 16)))
(cffi:defctype voicevox-style-id :uint32)

(cffi:defcstruct voicevox-synthesis-options
  (enable-interrogative-upspeak :bool))

(cffi:defcstruct voicevox-tts-options
  (enable-interrogative-upspeak :bool))

(cffi:defcstruct voicevox-user-dict-word
  (surface (:pointer :char))
  (pronunciation (:pointer :char))
  (accent-type :uintptr)
  (word-type voicevox-user-dict-word-type)
  (priority :uint8))

(cffi:defcfun ("voicevox_get_onnxruntime_lib_recommended_versioned_filename" vv-get-onnxruntime-lib-recommended-versioned-filename) :string)
(cffi:defcfun ("voicevox_get_onnxruntime_lib_recommended_unversioned_filename" vv-get-onnxruntime-lib-recommended-unversioned-filename) :string)
(cffi:defcfun ("voicevox_get_onnxruntime_lib_min_required_minor_version" vv-get-onnxruntime-lib-min-required-minor-version) :uint32)
(cffi:defcfun ("voicevox_get_onnxruntime_lib_max_supported_minor_version" vv-get-onnxruntime-lib-max-supported-minor-version) :uint32)
(cffi:defcfun ("voicevox_make_default_load_onnxruntime_options" vv-make-default-load-onnxruntime-options)
    (:struct voicevox-load-onnxruntime-options))
(cffi:defcfun ("voicevox_onnxruntime_get" vv-onnxruntime-get) (:pointer (:struct voicevox-onnxruntime)))
(cffi:defcfun ("voicevox_onnxruntime_load_once" vv-onnxruntime-load-once) :int
  (options (:struct voicevox-load-onnxruntime-options))
  (out-onnxruntime (:pointer (:pointer (:struct voicevox-onnxruntime)))))
(cffi:defcfun ("voicevox_onnxruntime_init_once" vv-onnxruntime-init-once) voicevox-result-code
  (out-onnxruntime (:pointer (:pointer (:struct voicevox-onnxruntime)))))

(cffi:defcfun ("voicevox_open_jtalk_rc_new" vv-open-jtalk-rc-new) voicevox-result-code
  (open-jtalk-dic-dir (:pointer :char))
  (out-open-jtalk (:pointer (:pointer (:struct open-jtalk-rc)))))
(cffi:defcfun ("voicevox_open_jtalk_rc_use_user_dict" vv-open-jtalk-rc-use-user-dict) voicevox-result-code
  (open-jtalk (:pointer (:struct open-jtalk-rc)))
  (user-dict (:pointer (:struct voicevox-user-dict))))
(cffi:defcfun ("voicevox_open_jtalk_rc_analyze" vv-open-jtalk-rc-analyze) voicevox-result-code
  (open-jtalk (:pointer (:struct open-jtalk-rc)))
  (text (:pointer :char))
  (output-accent-phrases-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_open_jtalk_rc_delete" vv-open-jtalk-rc-delete) :void
  (open-jtalk (:pointer (:struct open-jtalk-rc))))

(cffi:defcfun ("voicevox_make_default_initialize_options" vv-make-default-initialize-options)
    (:struct voicevox-initialize-options))
(cffi:defcfun ("voicevox_get_version" vv-get-version) :string)

(cffi:defcfun ("voicevox_audio_query_create_from_accent_phrases" vv-audio-query-create-from-accent-phrases) voicevox-result-code
  (accent-phrases-json (:pointer :char))
  (output-audio-query-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_audio_query_validate" vv-audio-query-validate) voicevox-result-code
  (audio-query-json (:pointer :char)))
(cffi:defcfun ("voicevox_accent_phrase_validate" vv-accent-phrase-validate) voicevox-result-code
  (accent-phrase-json (:pointer :char)))
(cffi:defcfun ("voicevox_mora_validate" vv-mora-validate) voicevox-result-code
  (mora-json (:pointer :char)))
(cffi:defcfun ("voicevox_score_validate" vv-score-validate) voicevox-result-code
  (score-json (:pointer :char)))
(cffi:defcfun ("voicevox_note_validate" vv-note-validate) voicevox-result-code
  (note-json (:pointer :char)))
(cffi:defcfun ("voicevox_frame_audio_query_validate" vv-frame-audio-query-validate) voicevox-result-code
  (frame-audio-query-json (:pointer :char)))
(cffi:defcfun ("voicevox_frame_phoneme_validate" vv-frame-phoneme-validate) voicevox-result-code
  (frame-phoneme-json (:pointer :char)))
(cffi:defcfun ("voicevox_ensure_compatible" vv-ensure-compatible) voicevox-result-code
  (score-json (:pointer :char))
  (frame-audio-query-json (:pointer :char)))

(cffi:defcfun ("voicevox_voice_model_file_open" vv-voice-model-file-open) voicevox-result-code
  (path (:pointer :char))
  (out-model (:pointer (:pointer (:struct voicevox-voice-model-file)))))
(cffi:defcfun ("voicevox_voice_model_file_id" vv-voice-model-file-id) :void
  (model (:pointer (:struct voicevox-voice-model-file)))
  (output-voice-model-id (:pointer (:array :uint8 16))))
(cffi:defcfun ("voicevox_voice_model_file_create_metas_json" vv-voice-model-file-create-metas-json) (:pointer :char)
  (model (:pointer (:struct voicevox-voice-model-file))))
(cffi:defcfun ("voicevox_voice_model_file_delete" vv-voice-model-file-delete) :void
  (model (:pointer (:struct voicevox-voice-model-file))))

(cffi:defcfun ("voicevox_synthesizer_new" vv-synthesizer-new) :int
  (onnxruntime (:pointer (:struct voicevox-onnxruntime)))
  (open-jtalk (:pointer (:struct open-jtalk-rc)))
  (options (:struct voicevox-initialize-options))
  (out-synthesizer (:pointer (:pointer (:struct voicevox-synthesizer)))))
(cffi:defcfun ("voicevox_synthesizer_delete" vv-synthesizer-delete) :void
  (synthesizer (:pointer (:struct voicevox-synthesizer))))
(cffi:defcfun ("voicevox_make_default_load_voice_model_options" vv-make-default-load-voice-model-options) (:struct voicevox-load-voice-model-options))
(cffi:defcfun ("voicevox_synthesizer_load_voice_model" vv-synthesizer-load-voice-model) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (model (:pointer (:struct voicevox-voice-model-file)))
  (options (:struct voicevox-load-voice-model-options)))
(cffi:defcfun ("voicevox_synthesizer_unload_voice_model" vv-synthesizer-unload-voice-model) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (model-id voicevox-voice-model-id))
(cffi:defcfun ("voicevox_synthesizer_get_onnxruntime" vv-synthesizer-get-onnxruntime) (:pointer (:struct voicevox-onnxruntime))
  (synthesizer (:pointer (:struct voicevox-synthesizer))))
(cffi:defcfun ("voicevox_synthesizer_is_gpu_mode" vv-synthesizer-is-gpu-mode) :bool
  (synthesizer (:pointer (:struct voicevox-synthesizer))))
(cffi:defcfun ("voicevox_synthesizer_is_loaded_voice_model" vv-synthesizer-is-loaded-voice-model) :bool
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (model-id voicevox-voice-model-id))
(cffi:defcfun ("voicevox_synthesizer_create_metas_json" vv-synthesizer-create-metas-json) (:pointer :char)
  (synthesizer (:pointer (:struct voicevox-synthesizer))))
(cffi:defcfun ("voicevox_onnxruntime_create_supported_devices_json" vv-onnxruntime-create-supported-devices-json) voicevox-result-code
  (onnxruntime (:pointer (:struct voicevox-onnxruntime)))
  (output-supported-devices-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_synthesizer_create_audio_query_from_kana" vv-synthesizer-create-audio-query-from-kana) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (kana (:pointer :char))
  (style-id voicevox-style-id)
  (output-audio-query-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_synthesizer_create_audio_query" vv-synthesizer-create-audio-query) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (text (:pointer :char))
  (style-id voicevox-style-id)
  (output-audio-query-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_synthesizer_create_accent_phrases_from_kana" vv-synthesizer-create-accent-phrases-from-kana) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (kana (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrases-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_synthesizer_create_accent_phrases" vv-synthesizer-create-accent-phrases) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (text (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrases-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_synthesizer_replace_mora_data" vv-synthesizer-replace-mora-data) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (accent-phrases-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrases-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_synthesizer_replace_phoneme_length" vv-synthesizer-replace-phoneme-length) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (accent-phrases-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrases-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_synthesizer_replace_mora_pitch" vv-synthesizer-replace-mora-pitch) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (accent-phrases-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrases-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_make_default_synthesis_options" vv-make-default-synthesis-options)
    (:struct voicevox-synthesis-options))
(cffi:defcfun ("voicevox_synthesizer_synthesis" vv-synthesizer-synthesis) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (audio-query-json (:pointer :char))
  (style-id voicevox-style-id)
  (options (:struct voicevox-synthesis-options))
  (output-wav-length (:pointer :uintptr))
  (output-wav (:pointer (:pointer :uint8))))
(cffi:defcfun ("voicevox_make_default_tts_options" vv-make-default-tts-options)
    (:struct voicevox-tts-options))
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
(cffi:defcfun ("voicevox_synthesizer_create_sing_frame_audio_query" vv-synthesizer-create-sing-frame-audio-query) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (score-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-frame-audio-query-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_synthesizer_create_sing_frame_f0" vv-synthesizer-create-sing-frame-f0) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (score-json (:pointer :char))
  (frame-audio-query-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-f0-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_synthesizer_create_sing_frame_volume" vv-synthesizer-create-sing-frame-volume) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (score-json (:pointer :char))
  (frame-audio-query-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-volume-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_synthesizer_frame_synthesis" vv-synthesizer-frame-synthesis) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (frame-audio-query-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-wav-length (:pointer :uintptr))
  (output-wav (:pointer (:pointer :uint8))))

(cffi:defcfun ("voicevox_json_free" vv-json-free) :void
  (json (:pointer :char)))
(cffi:defcfun ("voicevox_wav_free" vv-wav-free) :void
  (wav (:pointer :uint8)))
(cffi:defcfun ("voicevox_error_result_to_message" vv-error-result-to-message) :string
  (result-code voicevox-result-code))

(cffi:defcfun ("voicevox_user_dict_word_make" vv-user-dict-word-make) (:struct voicevox-user-dict-word)
  (surface (:pointer :char))
  (pronunciation (:pointer :char))
  (accent-type :uintptr))
(cffi:defcfun ("voicevox_user_dict_new" vv-user-dict-new) (:pointer (:struct voicevox-user-dict)))
(cffi:defcfun ("voicevox_user_dict_load" vv-user-dict-load) voicevox-result-code
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (dict-path (:pointer :char)))
(cffi:defcfun ("voicevox_user_dict_add_word" vv-user-dict-add-word) voicevox-result-code
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (word (:pointer (:struct voicevox-user-dict-word)))
  (output-word-uuid (:pointer (:array :uint8 16))))
(cffi:defcfun ("voicevox_user_dict_update_word" vv-user-dict-update-word) voicevox-result-code
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (word-uuid (:pointer (:array :uint8 16)))
  (word (:pointer (:struct voicevox-user-dict-word))))
(cffi:defcfun ("voicevox_user_dict_remove_word" vv-user-dict-remove-word) voicevox-result-code
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (word-uuid (:pointer (:array :uint8 16))))
(cffi:defcfun ("voicevox_user_dict_to_json" vv-user-dict-to-json) voicevox-result-code
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (output-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_user_dict_import" vv-user-dict-import) voicevox-result-code
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (other-dict (:pointer (:struct voicevox-user-dict))))
(cffi:defcfun ("voicevox_user_dict_save" vv-user-dict-save) voicevox-result-code
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (path (:pointer :char)))
(cffi:defcfun ("voicevox_user_dict_delete" vv-user-dict-delete) :void
  (user-dict (:pointer (:struct voicevox-user-dict))))


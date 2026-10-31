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
   ;; import from types.lisp
   :uint8
   :uint16
   :uint32
   :voicevox-result-code-type
   :voicevox-acceleration-mode-type
   :voicevox-user-dict-word-type-type
   :voicevox-on-existing-voice-model-id-type

   ;; enums
   :voicevox-acceleration-mode
   :voicevox-on-existing-voice-model-id
   :voicevox-result-code
   :voicevox-user-dict-word-type

   ;; structs
   :open-jtalk-rc
   :voicevox-onnxruntime
   :voicevox-synthesizer
   :voicevox-user-dict
   :voicevox-voice-model-file
   :voicevox-load-onnxruntime-options
   :voicevox-load-onnxruntime-options-filename
   :voicevox-initialize-options
   :voicevox-load-voice-model-options
   :voicevox-synthesis-options
   :voicevox-audio-query-frame-length-options
   :vv-make-default-audio-query-frame-length-options
   :voicevox-audio-feature-slice
   :voicevox-tts-options
   :voicevox-user-dict-word

   ;; typedefs
   :voicevox-voice-model-id
   :voicevox-style-id

   ;; ONNX Runtime
   :vv-get-onnxruntime-lib-recommended-versioned-filename
   :vv-get-onnxruntime-lib-recommended-unversioned-filename
   :vv-get-onnxruntime-lib-min-required-minor-version
   :vv-get-onnxruntime-lib-max-supported-minor-version
   :vv-make-default-load-onnxruntime-options
   :vv-onnxruntime-get
   :vv-onnxruntime-load-once
   :vv-onnxruntime-init-once

   ;; Open JTalk
   :vv-open-jtalk-rc-new
   :vv-open-jtalk-rc-use-user-dict
   :vv-open-jtalk-rc-analyze
   :vv-open-jtalk-rc-delete

   ;; general
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

   ;; Voice model file
   :vv-voice-model-file-open
   :vv-voice-model-file-id
   :vv-voice-model-file-create-metas-json
   :vv-voice-model-file-delete

   ;; Synthesizer
   :vv-synthesizer-new
   :vv-synthesizer-delete
   :vv-make-default-audio-query-frame-length-options
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

   ;; memory
   :vv-string-free
   :vv-bytes-free
   :vv-json-free ;; deprecated
   :vv-wav-free  ;; deprecated
   :vv-error-result-to-message

   ;; global variable
   :+voicevox-empty-bytes+

   ;; for Steaming function
   :vv-get-frame-rate
   :vv-audio-query-frame-length
   :vv-wav-from-s16le
   :vv-synthesizer-create-audio-feature
   :vv-audio-feature-len
   :vv-audio-feature-is-empty
   :vv-audio-feature-slice
   :vv-audio-feature-slice-new
   :vv-audio-feature-slice-len
   :vv-audio-feature-slice-is-empty
   :vv-audio-feature-slice-slice
   :vv-synthesizer-render
   :vv-audio-feature-delete

   ;; User dictionary
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
  (:voicevox-result-incompatible-queries-error 35)
  (:voicevox-result-start-out-of-bounds-error 36)
  (:voicevox-result-end-out-of-bounds-error 37)
  (:voicevox-result-start-exceeds-end-error 38))

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
(cffi:defcstruct voicevox-audio-feature)

(cffi:defcstruct voicevox-load-onnxruntime-options
    (voicevox-load-onnxruntime-options-filename :pointer))

(cffi:defcstruct voicevox-initialize-options
    (acceleration-mode voicevox-acceleration-mode)
  (cpu-num-threads :uint16))

(cffi:defcstruct voicevox-load-voice-model-options
    (on-existing voicevox-on-existing-voice-model-id))

(cffi:defcstruct voicevox-audio-query-frame-length-options
    (enable-interrogative-upspeak :bool))
(cffi:defcstruct voicevox-audio-feature-slice
  (audio-feature (:pointer (:struct voicevox-audio-feature)))
  (start-offset :uintptr)
  (end :uintptr)
  (end-is-inclusive :int ;; correctly types is bool, but cffi bugs not passed bool in struct. therefore instead int. ref: https://github.com/cffi/cffi/issues/265
                    )
  (clamp :int ;; correctly types is bool, but cffi bugs not passed bool in struct. therefore instead int. ref: https://github.com/cffi/cffi/issues/265
         )
  (allow-start-exceeds-end :int))

(cffi:defctype voicevox-voice-model-id (:pointer (:array :uint8 16)))
(cffi:defctype voicevox-style-id :uint32)

(cffi:defcstruct voicevox-synthesis-options
    (enable-interrogative-upspeak :int ;; correctly types is bool, but cffi bugs not passed bool in struct. therefore instead int. ref: https://github.com/cffi/cffi/issues/265
                                  ))

(cffi:defcstruct voicevox-tts-options
  (enable-interrogative-upspeak :int ;; correctly types is bool, but cffi bugs not passed bool in struct. therefore instead int. ref: https://github.com/cffi/cffi/issues/265
                                ))

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
(cffi:defcfun ("voicevox_onnxruntime_init_once" vv-onnxruntime-init-once) :int
  (out-onnxruntime (:pointer (:pointer (:struct voicevox-onnxruntime)))))

(cffi:defcfun ("voicevox_open_jtalk_rc_new" vv-open-jtalk-rc-new) :int
  (open-jtalk-dic-dir (:pointer :char))
  (out-open-jtalk (:pointer (:pointer (:struct open-jtalk-rc)))))
(cffi:defcfun ("voicevox_open_jtalk_rc_use_user_dict" vv-open-jtalk-rc-use-user-dict) :int
  (open-jtalk (:pointer (:struct open-jtalk-rc)))
  (user-dict (:pointer (:struct voicevox-user-dict))))
(cffi:defcfun ("voicevox_open_jtalk_rc_analyze" vv-open-jtalk-rc-analyze) :int
  (open-jtalk (:pointer (:struct open-jtalk-rc)))
  (text (:pointer :char))
  (output-accent-phrases-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_open_jtalk_rc_delete" vv-open-jtalk-rc-delete) :void
  (open-jtalk (:pointer (:struct open-jtalk-rc))))

(cffi:defcfun ("voicevox_make_default_initialize_options" vv-make-default-initialize-options)
    (:struct voicevox-initialize-options))
(cffi:defcfun ("voicevox_get_version" vv-get-version) :string)

(cffi:defcfun ("voicevox_audio_query_create_from_accent_phrases" vv-audio-query-create-from-accent-phrases) :int
  (accent-phrases-json (:pointer :char))
  (output-audio-query-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_audio_query_validate" vv-audio-query-validate) :int
  (audio-query-json (:pointer :char)))
(cffi:defcfun ("voicevox_accent_phrase_validate" vv-accent-phrase-validate) :int
  (accent-phrase-json (:pointer :char)))
(cffi:defcfun ("voicevox_mora_validate" vv-mora-validate) :int
  (mora-json (:pointer :char)))
(cffi:defcfun ("voicevox_score_validate" vv-score-validate) :int
  (score-json (:pointer :char)))
(cffi:defcfun ("voicevox_note_validate" vv-note-validate) :int
  (note-json (:pointer :char)))
(cffi:defcfun ("voicevox_frame_audio_query_validate" vv-frame-audio-query-validate) :int
  (frame-audio-query-json (:pointer :char)))
(cffi:defcfun ("voicevox_frame_phoneme_validate" vv-frame-phoneme-validate) :int
  (frame-phoneme-json (:pointer :char)))
(cffi:defcfun ("voicevox_ensure_compatible" vv-ensure-compatible) :int
  (score-json (:pointer :char))
  (frame-audio-query-json (:pointer :char)))

(cffi:defcfun ("voicevox_voice_model_file_open" vv-voice-model-file-open) :int
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
(cffi:defcfun ("voicevox_synthesizer_load_voice_model" vv-synthesizer-load-voice-model) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (model (:pointer (:struct voicevox-voice-model-file)))
  (options (:struct voicevox-load-voice-model-options)))
(cffi:defcfun ("voicevox_synthesizer_unload_voice_model" vv-synthesizer-unload-voice-model) :int
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
(cffi:defcfun ("voicevox_onnxruntime_create_supported_devices_json" vv-onnxruntime-create-supported-devices-json) :int
  (onnxruntime (:pointer (:struct voicevox-onnxruntime)))
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
  (output-audio-query-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_synthesizer_create_accent_phrases_from_kana" vv-synthesizer-create-accent-phrases-from-kana) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (kana (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrases-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_synthesizer_create_accent_phrases" vv-synthesizer-create-accent-phrases) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (text (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrases-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_synthesizer_replace_mora_data" vv-synthesizer-replace-mora-data) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (accent-phrases-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrases-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_synthesizer_replace_phoneme_length" vv-synthesizer-replace-phoneme-length) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (accent-phrases-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrases-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_synthesizer_replace_mora_pitch" vv-synthesizer-replace-mora-pitch) :int
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
(cffi:defcfun ("voicevox_synthesizer_create_sing_frame_audio_query" vv-synthesizer-create-sing-frame-audio-query) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (score-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-frame-audio-query-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_synthesizer_create_sing_frame_f0" vv-synthesizer-create-sing-frame-f0) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (score-json (:pointer :char))
  (frame-audio-query-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-f0-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_synthesizer_create_sing_frame_volume" vv-synthesizer-create-sing-frame-volume) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (score-json (:pointer :char))
  (frame-audio-query-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-volume-json (:pointer (:pointer :char))))
(cffi:defcfun ("voicevox_synthesizer_frame_synthesis" vv-synthesizer-frame-synthesis) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (frame-audio-query-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-wav-length (:pointer :uintptr))
  (output-wav (:pointer (:pointer :uint8))))

(cffi:defcfun ("voicevox_string_free" vv-string-free) :void
  (string (:pointer :char)))
(cffi:defcfun ("voicevox_bytes_free" vv-bytes-free) :void
  (bytes (:pointer :uint8)))
;; voicevox_json_free is deprecated since 0.17.1
(cffi:defcfun ("voicevox_json_free" vv-json-free) :void
  (json (:pointer :char)))
;; voicevox_wav_free is depreacated since 0.17.1
(cffi:defcfun ("voicevox_wav_free" vv-wav-free) :void
  (wav (:pointer :uint8)))
(cffi:defcfun ("voicevox_error_result_to_message" vv-error-result-to-message) :string
  (result-code :int))

(cffi:defcfun ("voicevox_user_dict_word_make" vv-user-dict-word-make) (:struct voicevox-user-dict-word)
  (surface (:pointer :char))
  (pronunciation (:pointer :char))
  (accent-type :uintptr))
(cffi:defcfun ("voicevox_user_dict_new" vv-user-dict-new) (:pointer (:struct voicevox-user-dict)))
(cffi:defcfun ("voicevox_user_dict_load" vv-user-dict-load) :int
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (dict-path (:pointer :char)))
(cffi:defcfun ("voicevox_user_dict_add_word" vv-user-dict-add-word) :int
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

(cffi:defcvar ("voicevox_empty_bytes" +voicevox-empty-bytes+ :read-only t) (:pointer :uint8))

(cffi:defcfun ("voicevox_make_default_audio_query_frame_length_options" vv-make-default-audio-query-frame-length-options) (:struct voicevox-audio-query-frame-length-options))
(cffi:defcfun ("voicevox_get_frame_rate" vv-get-frame-rate) :double)
(cffi:defcfun ("voicevox_audio_query_frame_length" vv-audio-query-frame-length) :int
  (audio-query-json (:pointer :char))
  (options (:struct voicevox-audio-query-frame-length-options))
  (output-frame-length (:pointer :uintptr)))
(cffi:defcfun ("voicevox_wav_from_s16le" vv-wav-from-s16le) :void
  (pcm-length :uintptr)
  (pcm (:pointer :uint8))
  (sampling-rate :uint32)
  (is-stereo :bool)
  (output-wav-length (:pointer :uintptr))
  (output-wav (:pointer (:pointer :uint8))))

(cffi:defcfun ("voicevox_synthesizer_create_audio_feature" vv-synthesizer-create-audio-feature) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (audio-query-json (:pointer :char))
  (style-id voicevox-style-id)
  (options (:struct voicevox-synthesis-options))
  (out-audio-features (:pointer (:pointer (:struct voicevox-audio-feature)))))

(cffi:defcfun ("voicevox_audio_feature_len" vv-audio-feature-len) :uintptr
  (audio-feature (:pointer (:struct voicevox-audio-feature))))

(cffi:defcfun ("voicevox_audio_feature_is_empty" vv-audio-feature-is-empty) :bool
  (audio-feature (:pointer (:struct voicevox-audio-feature))))

(cffi:defcfun ("voicevox_audio_feature_slice" vv-audio-feature-slice) :int
  (audio-feature (:pointer (:struct voicevox-audio-feature)))
  (start-inclusive :uintptr)
  (end :uintptr)
  (end-is-include :bool)
  (clamp :bool)
  (allow-start-exceeds-end :bool)
  (output (:pointer (:struct voicevox-audio-feature-slice))))

(cffi:defcfun ("voicevox_audio_feature_slice_new" vv-audio-feature-slice-new) (:struct voicevox-audio-feature-slice)
  (audio-feature (:pointer (:struct voicevox-audio-feature)))
  (clamp :bool)
  (allow-start-exceeds-end :bool))

(cffi:defcfun ("voicevox_audio_feature_slice_len" vv-audio-feature-slice-len) :int
  (audio-feature-slice (:struct voicevox-audio-feature-slice))
  (output (:pointer :uintptr)))

(cffi:defcfun ("voicevox_audio_feature_slice_is_empty" vv-audio-feature-slice-is-empty) :int
  (audio-feature-slice (:struct voicevox-audio-feature-slice))
  (output (:pointer :bool)))

(cffi:defcfun ("voicevox_audio_feature_slice_slice" vv-audio-feature-slice-slice) :int
  (audio-feature-slice (:struct voicevox-audio-feature-slice))
  (start-inclusive :uintptr)
  (end :uintptr)
  (end-is-inclusive :bool)
  (clamp :bool)
  (allow-start-exceeds-end :bool)
  (output (:pointer (:struct voicevox-audio-feature-slice))))

(cffi:defcfun ("voicevox_synthesizer_render" vv-synthesizer-render) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (audio-feature-slice (:struct voicevox-audio-feature-slice))
  (output-pcm-length (:pointer :uintptr))
  (output-pcm (:pointer (:pointer :uint8))))

(cffi:defcfun ("voicevox_audio_feature_delete" vv-audio-feature-delete) :void
  (audio-feature (:pointer (:struct voicevox-audio-feature))))

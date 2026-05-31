(in-package :cl-user)

(defpackage cl-unofficial-voicevox-core-wrapper
  (:use :cl :cffi)
  (:nicknames :unofficial-vv-core-wrapper)
  (:import-from
   :cl-unofficial-voicevox-core-wrapper.types
   :uint8
   :uint16
   :uint32
   :voicevox-result-code-type
   :voicevox-acceleration-mode-type
   :voicevox-user-dict-word-type-type)
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
   :voicevox-synthesis-options
   :voicevox-tts-options
   :voicevox-user-dict-word
   :voicevox-style-id
   :voicevox-voice-model-id
   :load-library
   :close-library
   :get-version
   :get-result-from-code
   :error-result-to-message
   :get-onnxruntime-lib-versioned-filename
   :get-onnxruntime-lib-unversioned-filename
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

(in-package :cl-unofficial-voicevox-core-wrapper)

(defvar *loaded-libraries* (make-hash-table :test #'equal))

(defcenum voicevox-acceleration-mode
  (:voicevox-acceleration-mode-auto 0)
  (:voicevox-acceleration-mode-cpu 1)
  (:voicevox-acceleration-mode-gpu 2))

(defcenum voicevox-result-code
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
  (:voicevox-result-invalid-model-header-error 28)
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

(defcenum voicevox-user-dict-word-type
  (:voicevox-user-dict-word-type-proper-noun 0)
  (:voicevox-user-dict-word-type-common-noun 1)
  (:voicevox-user-dict-word-type-verb 2)
  (:voicevox-user-dict-word-type-adjective 3)
  (:voicevox-user-dict-word-type-suffix 4))

(defcstruct open-jtalk-rc)
(defcstruct voicevox-onnxruntime)
(defcstruct voicevox-synthesizer)
(defcstruct voicevox-user-dict)
(defcstruct voicevox-voice-model-file)

(defcstruct voicevox-load-onnxruntime-options
  (filename (:pointer :char)))

(defcstruct voicevox-initialize-options
  (acceleration-mode voicevox-acceleration-mode)
  (cpu-num-threads :uint16))

(defctype voicevox-voice-model-id (:pointer (:array :uint8 16)))
(defctype voicevox-style-id :uint32)

(defcstruct voicevox-synthesis-options
  (enable-interrogative-upspeak :bool))

(defcstruct voicevox-tts-options
  (enable-interrogative-upspeak :bool))

(defcstruct voicevox-user-dict-word
  (surface (:pointer :char))
  (pronunciation (:pointer :char))
  (accent-type :uintptr)
  (word-type voicevox-user-dict-word-type)
  (priority :uint32))

(defcfun ("voicevox_get_onnxruntime_lib_versioned_filename" vv-get-onnxruntime-lib-versioned-filename) :string)
(defcfun ("voicevox_get_onnxruntime_lib_unversioned_filename" vv-get-onnxruntime-lib-unversioned-filename) :string)
(defcfun ("voicevox_make_default_load_onnxruntime_options" vv-make-default-load-onnxruntime-options)
    (:struct voicevox-load-onnxruntime-options))
(defcfun ("voicevox_onnxruntime_get" vv-onnxruntime-get) (:pointer (:struct voicevox-onnxruntime)))
(defcfun ("voicevox_onnxruntime_load_once" vv-onnxruntime-load-once) :int
  (options (:struct voicevox-load-onnxruntime-options))
  (out-onnxruntime (:pointer (:pointer (:struct voicevox-onnxruntime)))))
(defcfun ("voicevox_onnxruntime_init_once" vv-onnxruntime-init-once) voicevox-result-code
  (out-onnxruntime (:pointer (:pointer (:struct voicevox-onnxruntime)))))

(defcfun ("voicevox_open_jtalk_rc_new" vv-open-jtalk-rc-new) voicevox-result-code
  (open-jtalk-dic-dir (:pointer :char))
  (out-open-jtalk (:pointer (:pointer (:struct open-jtalk-rc)))))
(defcfun ("voicevox_open_jtalk_rc_use_user_dict" vv-open-jtalk-rc-use-user-dict) voicevox-result-code
  (open-jtalk (:pointer (:struct open-jtalk-rc)))
  (user-dict (:pointer (:struct voicevox-user-dict))))
(defcfun ("voicevox_open_jtalk_rc_analyze" vv-open-jtalk-rc-analyze) voicevox-result-code
  (open-jtalk (:pointer (:struct open-jtalk-rc)))
  (text (:pointer :char))
  (output-accent-phrases-json (:pointer (:pointer :char))))
(defcfun ("voicevox_open_jtalk_rc_delete" vv-open-jtalk-rc-delete) :void
  (open-jtalk (:pointer (:struct open-jtalk-rc))))

(defcfun ("voicevox_make_default_initialize_options" vv-make-default-initialize-options)
    (:struct voicevox-initialize-options))
(defcfun ("voicevox_get_version" vv-get-version) :string)

(defcfun ("voicevox_audio_query_create_from_accent_phrases" vv-audio-query-create-from-accent-phrases) voicevox-result-code
  (accent-phrases-json (:pointer :char))
  (output-audio-query-json (:pointer (:pointer :char))))
(defcfun ("voicevox_audio_query_validate" vv-audio-query-validate) voicevox-result-code
  (audio-query-json (:pointer :char)))
(defcfun ("voicevox_accent_phrase_validate" vv-accent-phrase-validate) voicevox-result-code
  (accent-phrase-json (:pointer :char)))
(defcfun ("voicevox_mora_validate" vv-mora-validate) voicevox-result-code
  (mora-json (:pointer :char)))
(defcfun ("voicevox_score_validate" vv-score-validate) voicevox-result-code
  (score-json (:pointer :char)))
(defcfun ("voicevox_note_validate" vv-note-validate) voicevox-result-code
  (note-json (:pointer :char)))
(defcfun ("voicevox_frame_audio_query_validate" vv-frame-audio-query-validate) voicevox-result-code
  (frame-audio-query-json (:pointer :char)))
(defcfun ("voicevox_frame_phoneme_validate" vv-frame-phoneme-validate) voicevox-result-code
  (frame-phoneme-json (:pointer :char)))
(defcfun ("voicevox_ensure_compatible" vv-ensure-compatible) voicevox-result-code
  (score-json (:pointer :char))
  (frame-audio-query-json (:pointer :char)))

(defcfun ("voicevox_voice_model_file_open" vv-voice-model-file-open) voicevox-result-code
  (path (:pointer :char))
  (out-model (:pointer (:pointer (:struct voicevox-voice-model-file)))))
(defcfun ("voicevox_voice_model_file_id" vv-voice-model-file-id) :void
  (model (:pointer (:struct voicevox-voice-model-file)))
  (output-voice-model-id (:pointer (:array :uint8 16))))
(defcfun ("voicevox_voice_model_file_create_metas_json" vv-voice-model-file-create-metas-json) (:pointer :char)
  (model (:pointer (:struct voicevox-voice-model-file))))
(defcfun ("voicevox_voice_model_file_delete" vv-voice-model-file-delete) :void
  (model (:pointer (:struct voicevox-voice-model-file))))

(defcfun ("voicevox_synthesizer_new" vv-synthesizer-new) :int
  (onnxruntime (:pointer (:struct voicevox-onnxruntime)))
  (open-jtalk (:pointer (:struct open-jtalk-rc)))
  (options (:struct voicevox-initialize-options))
  (out-synthesizer (:pointer (:pointer (:struct voicevox-synthesizer)))))
(defcfun ("voicevox_synthesizer_delete" vv-synthesizer-delete) :void
  (synthesizer (:pointer (:struct voicevox-synthesizer))))
(defcfun ("voicevox_synthesizer_load_voice_model" vv-synthesizer-load-voice-model) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (model (:pointer (:struct voicevox-voice-model-file))))
(defcfun ("voicevox_synthesizer_unload_voice_model" vv-synthesizer-unload-voice-model) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (model-id voicevox-voice-model-id))
(defcfun ("voicevox_synthesizer_get_onnxruntime" vv-synthesizer-get-onnxruntime) (:pointer (:struct voicevox-onnxruntime))
  (synthesizer (:pointer (:struct voicevox-synthesizer))))
(defcfun ("voicevox_synthesizer_is_gpu_mode" vv-synthesizer-is-gpu-mode) :bool
  (synthesizer (:pointer (:struct voicevox-synthesizer))))
(defcfun ("voicevox_synthesizer_is_loaded_voice_model" vv-synthesizer-is-loaded-voice-model) :bool
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (model-id voicevox-voice-model-id))
(defcfun ("voicevox_synthesizer_create_metas_json" vv-synthesizer-create-metas-json) (:pointer :char)
  (synthesizer (:pointer (:struct voicevox-synthesizer))))
(defcfun ("voicevox_onnxruntime_create_supported_devices_json" vv-onnxruntime-create-supported-devices-json) voicevox-result-code
  (onnxruntime (:pointer (:struct voicevox-onnxruntime)))
  (output-supported-devices-json (:pointer (:pointer :char))))
(defcfun ("voicevox_synthesizer_create_audio_query_from_kana" vv-synthesizer-create-audio-query-from-kana) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (kana (:pointer :char))
  (style-id voicevox-style-id)
  (output-audio-query-json (:pointer (:pointer :char))))
(defcfun ("voicevox_synthesizer_create_audio_query" vv-synthesizer-create-audio-query) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (text (:pointer :char))
  (style-id voicevox-style-id)
  (output-audio-query-json (:pointer (:pointer :char))))
(defcfun ("voicevox_synthesizer_create_accent_phrases_from_kana" vv-synthesizer-create-accent-phrases-from-kana) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (kana (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrases-json (:pointer (:pointer :char))))
(defcfun ("voicevox_synthesizer_create_accent_phrases" vv-synthesizer-create-accent-phrases) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (text (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrases-json (:pointer (:pointer :char))))
(defcfun ("voicevox_synthesizer_replace_mora_data" vv-synthesizer-replace-mora-data) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (accent-phrases-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrases-json (:pointer (:pointer :char))))
(defcfun ("voicevox_synthesizer_replace_phoneme_length" vv-synthesizer-replace-phoneme-length) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (accent-phrases-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrases-json (:pointer (:pointer :char))))
(defcfun ("voicevox_synthesizer_replace_mora_pitch" vv-synthesizer-replace-mora-pitch) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (accent-phrases-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-accent-phrases-json (:pointer (:pointer :char))))
(defcfun ("voicevox_make_default_synthesis_options" vv-make-default-synthesis-options)
    (:struct voicevox-synthesis-options))
(defcfun ("voicevox_synthesizer_synthesis" vv-synthesizer-synthesis) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (audio-query-json (:pointer :char))
  (style-id voicevox-style-id)
  (options (:struct voicevox-synthesis-options))
  (output-wav-length (:pointer :uintptr))
  (output-wav (:pointer (:pointer :uint8))))
(defcfun ("voicevox_make_default_tts_options" vv-make-default-tts-options)
    (:struct voicevox-tts-options))
(defcfun ("voicevox_synthesizer_tts_from_kana" vv-synthesizer-tts-from-kana) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (kana (:pointer :char))
  (style-id voicevox-style-id)
  (options (:struct voicevox-tts-options))
  (output-wav-length (:pointer :uintptr))
  (output-wav (:pointer (:pointer :uint8))))
(defcfun ("voicevox_synthesizer_tts" vv-synthesizer-tts) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (text (:pointer :char))
  (style-id voicevox-style-id)
  (options (:struct voicevox-tts-options))
  (output-wav-length (:pointer :uintptr))
  (output-wav (:pointer (:pointer :uint8))))
(defcfun ("voicevox_synthesizer_create_sing_frame_audio_query" vv-synthesizer-create-sing-frame-audio-query) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (score-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-frame-audio-query-json (:pointer (:pointer :char))))
(defcfun ("voicevox_synthesizer_create_sing_frame_f0" vv-synthesizer-create-sing-frame-f0) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (score-json (:pointer :char))
  (frame-audio-query-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-f0-json (:pointer (:pointer :char))))
(defcfun ("voicevox_synthesizer_create_sing_frame_volume" vv-synthesizer-create-sing-frame-volume) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (score-json (:pointer :char))
  (frame-audio-query-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-volume-json (:pointer (:pointer :char))))
(defcfun ("voicevox_synthesizer_frame_synthesis" vv-synthesizer-frame-synthesis) voicevox-result-code
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (frame-audio-query-json (:pointer :char))
  (style-id voicevox-style-id)
  (output-wav-length (:pointer :uintptr))
  (output-wav (:pointer (:pointer :uint8))))

(defcfun ("voicevox_json_free" vv-json-free) :void
  (json (:pointer :char)))
(defcfun ("voicevox_wav_free" vv-wav-free) :void
  (wav (:pointer :uint8)))
(defcfun ("voicevox_error_result_to_message" vv-error-result-to-message) :string
  (result-code voicevox-result-code))

(defcfun ("voicevox_user_dict_word_make" vv-user-dict-word-make) (:struct voicevox-user-dict-word)
  (surface (:pointer :char))
  (pronunciation (:pointer :char))
  (accent-type :uintptr))
(defcfun ("voicevox_user_dict_new" vv-user-dict-new) (:pointer (:struct voicevox-user-dict)))
(defcfun ("voicevox_user_dict_load" vv-user-dict-load) voicevox-result-code
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (dict-path (:pointer :char)))
(defcfun ("voicevox_user_dict_add_word" vv-user-dict-add-word) voicevox-result-code
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (word (:pointer (:struct voicevox-user-dict-word)))
  (output-word-uuid (:pointer (:array :uint8 16))))
(defcfun ("voicevox_user_dict_update_word" vv-user-dict-update-word) voicevox-result-code
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (word-uuid (:pointer (:array :uint8 16)))
  (word (:pointer (:struct voicevox-user-dict-word))))
(defcfun ("voicevox_user_dict_remove_word" vv-user-dict-remove-word) voicevox-result-code
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (word-uuid (:pointer (:array :uint8 16))))
(defcfun ("voicevox_user_dict_to_json" vv-user-dict-to-json) voicevox-result-code
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (output-json (:pointer (:pointer :char))))
(defcfun ("voicevox_user_dict_import" vv-user-dict-import) voicevox-result-code
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (other-dict (:pointer (:struct voicevox-user-dict))))
(defcfun ("voicevox_user_dict_save" vv-user-dict-save) voicevox-result-code
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (path (:pointer :char)))
(defcfun ("voicevox_user_dict_delete" vv-user-dict-delete) :void
  (user-dict (:pointer (:struct voicevox-user-dict))))

(defun load-library (path)
  (let ((library (load-foreign-library path)))
    (setf (gethash path *loaded-libraries*) library)
    library))

(defun close-library (library-or-path)
  (let ((library (if (stringp library-or-path)
                     (gethash library-or-path *loaded-libraries*)
                     library-or-path)))
    (when library
      (close-foreign-library library)
      (when (stringp library-or-path)
        (remhash library-or-path *loaded-libraries*)))
    library))

(defun get-version ()
  (vv-get-version))

(defun get-result-from-code (code)
  (etypecase code
    (keyword code)
    (integer (foreign-enum-keyword 'voicevox-result-code code))))

(defun error-result-to-message (result-code)
  (vv-error-result-to-message result-code))

(defun get-onnxruntime-lib-versioned-filename ()
  (vv-get-onnxruntime-lib-versioned-filename))

(defun get-onnxruntime-lib-unversioned-filename ()
  (vv-get-onnxruntime-lib-unversioned-filename))

(defun pointer-value (pointer type)
  (mem-ref pointer type))

(defun json-pointer-to-string-and-free (pointer)
  (unwind-protect
       (foreign-string-to-lisp pointer)
    (vv-json-free pointer)))

(defun copy-wav-pointer (pointer length)
  (let ((result (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length result)
      (setf (aref result i) (mem-aref pointer :uint8 i)))))

(defun make-array-from-pointer (target length pointer-type value-type)
  (let ((result (make-array length)))
    (dotimes (i length result)
      (setf (aref result i)
            (mem-aref (mem-aref target pointer-type) value-type i)))))

(defclass onnxruntime-class ()
  ((onnxruntime-ptr
    :accessor onnxruntime-ptr
    :initform (foreign-alloc '(:pointer (:struct voicevox-onnxruntime))))))

(defmethod onnxruntime-init ((self onnxruntime-class) &optional options)
  (get-result-from-code
   (vv-onnxruntime-load-once (or options (vv-make-default-load-onnxruntime-options))
                             (onnxruntime-ptr self))))

(defun onnxruntime-get ()
  (vv-onnxruntime-get))

(defclass open-jtalk-rc-class ()
  ((open-jtalk-rc-ptr
    :accessor open-jtalk-rc-ptr
    :initform (foreign-alloc '(:pointer (:struct open-jtalk-rc))))))

(defmethod open-jtalk-rc-init ((self open-jtalk-rc-class) open-jtalk-dic-dir)
  (declare (type string open-jtalk-dic-dir))
  (with-foreign-string (c-open-jtalk-dic-dir open-jtalk-dic-dir)
    (vv-open-jtalk-rc-new c-open-jtalk-dic-dir (open-jtalk-rc-ptr self))))

(defmethod open-jtalk-rc-delete ((self open-jtalk-rc-class))
  (vv-open-jtalk-rc-delete
   (pointer-value (open-jtalk-rc-ptr self) '(:pointer (:struct open-jtalk-rc)))))

(defmethod open-jtalk-rc-close ((self open-jtalk-rc-class))
  (unless (null-pointer-p (open-jtalk-rc-ptr self))
    (open-jtalk-rc-delete self)
    (foreign-free (open-jtalk-rc-ptr self))
    (setf (slot-value self 'open-jtalk-rc-ptr) (null-pointer))))

(defmethod open-jtalk-rc-use-user-dict ((self open-jtalk-rc-class) user-dict)
  (vv-open-jtalk-rc-use-user-dict
   (pointer-value (open-jtalk-rc-ptr self) '(:pointer (:struct open-jtalk-rc)))
   (user-dict-ptr user-dict)))

(defmethod open-jtalk-rc-analyze ((self open-jtalk-rc-class) text)
  (with-foreign-string (c-text text)
    (with-foreign-object (output-accent-phrases-json '(:pointer :char))
      (let ((result (vv-open-jtalk-rc-analyze
                     (pointer-value (open-jtalk-rc-ptr self) '(:pointer (:struct open-jtalk-rc)))
                     c-text
                     output-accent-phrases-json)))
        (values result
                (when (eq result :voicevox-result-ok)
                  (json-pointer-to-string-and-free
                   (mem-ref output-accent-phrases-json '(:pointer :char)))))))))

(defclass voicevox-class ()
  ((options :accessor options :initform (vv-make-default-initialize-options))
   (synthesizer
    :accessor synthesizer
    :initform (foreign-alloc '(:pointer (:struct voicevox-synthesizer))))))

(defmethod voicevox-synthesizer-init ((self voicevox-class) onnxruntime-instance open-jtalk-instance)
  (get-result-from-code
   (vv-synthesizer-new
    (pointer-value (onnxruntime-ptr onnxruntime-instance) '(:pointer (:struct voicevox-onnxruntime)))
    (pointer-value (open-jtalk-rc-ptr open-jtalk-instance) '(:pointer (:struct open-jtalk-rc)))
    (options self)
    (synthesizer self))))

(defmethod voicevox-synthesizer-delete ((self voicevox-class))
  (vv-synthesizer-delete
   (pointer-value (synthesizer self) '(:pointer (:struct voicevox-synthesizer)))))

(defun synthesizer-pointer (voicevox)
  (pointer-value (synthesizer voicevox) '(:pointer (:struct voicevox-synthesizer))))

(defmethod voicevox-synthesizer-load-voice-model ((self voicevox-class) voice-model-file)
  (vv-synthesizer-load-voice-model (synthesizer-pointer self)
                                   (voice-model-file-pointer voice-model-file)))

(defmethod voicevox-synthesizer-unload-voice-model ((self voicevox-class) model-id)
  (vv-synthesizer-unload-voice-model (synthesizer-pointer self) model-id))

(defmethod voicevox-synthesizer-is-gpu-mode ((self voicevox-class))
  (vv-synthesizer-is-gpu-mode (synthesizer-pointer self)))

(defmethod voicevox-synthesizer-is-loaded-voice-model ((self voicevox-class) model-id)
  (vv-synthesizer-is-loaded-voice-model (synthesizer-pointer self) model-id))

(defmethod voicevox-synthesizer-create-metas-json ((self voicevox-class))
  (json-pointer-to-string-and-free
   (vv-synthesizer-create-metas-json (synthesizer-pointer self))))

(defmacro call-json-output-function (function &rest args)
  (let ((output-json (gensym "OUTPUT-JSON-"))
        (result (gensym "RESULT-"))
        (function-name (if (and (consp function) (eq (first function) 'function))
                           (second function)
                           function)))
    `(with-foreign-object (,output-json '(:pointer :char))
       (let ((,result (,function-name ,@args ,output-json)))
         (values ,result
                 (when (eq ,result :voicevox-result-ok)
                   (json-pointer-to-string-and-free
                    (mem-ref ,output-json '(:pointer :char)))))))))

(defmethod voicevox-synthesizer-create-audio-query ((self voicevox-class) text style-id)
  (with-foreign-string (c-text text)
    (call-json-output-function #'vv-synthesizer-create-audio-query
                               (synthesizer-pointer self)
                               c-text
                               style-id)))

(defmethod voicevox-synthesizer-create-audio-query-from-kana ((self voicevox-class) kana style-id)
  (with-foreign-string (c-kana kana)
    (call-json-output-function #'vv-synthesizer-create-audio-query-from-kana
                               (synthesizer-pointer self)
                               c-kana
                               style-id)))

(defmethod voicevox-synthesizer-create-accent-phrases ((self voicevox-class) text style-id)
  (with-foreign-string (c-text text)
    (call-json-output-function #'vv-synthesizer-create-accent-phrases
                               (synthesizer-pointer self)
                               c-text
                               style-id)))

(defmethod voicevox-synthesizer-create-accent-phrases-from-kana ((self voicevox-class) kana style-id)
  (with-foreign-string (c-kana kana)
    (call-json-output-function #'vv-synthesizer-create-accent-phrases-from-kana
                               (synthesizer-pointer self)
                               c-kana
                               style-id)))

(defmethod voicevox-synthesizer-replace-mora-data ((self voicevox-class) accent-phrases-json style-id)
  (with-foreign-string (c-json accent-phrases-json)
    (call-json-output-function #'vv-synthesizer-replace-mora-data
                               (synthesizer-pointer self)
                               c-json
                               style-id)))

(defmethod voicevox-synthesizer-replace-phoneme-length ((self voicevox-class) accent-phrases-json style-id)
  (with-foreign-string (c-json accent-phrases-json)
    (call-json-output-function #'vv-synthesizer-replace-phoneme-length
                               (synthesizer-pointer self)
                               c-json
                               style-id)))

(defmethod voicevox-synthesizer-replace-mora-pitch ((self voicevox-class) accent-phrases-json style-id)
  (with-foreign-string (c-json accent-phrases-json)
    (call-json-output-function #'vv-synthesizer-replace-mora-pitch
                               (synthesizer-pointer self)
                               c-json
                               style-id)))

(defun call-wav-output-function (function &rest args)
  (with-foreign-object (output-wav-length :uintptr)
    (with-foreign-object (output-wav '(:pointer :uint8))
      (let ((result (apply function (append args (list output-wav-length output-wav)))))
        (setf result (get-result-from-code result))
        (if (eq result :voicevox-result-ok)
            (let* ((wav-length (mem-ref output-wav-length :uintptr))
                   (wav-pointer (mem-ref output-wav '(:pointer :uint8)))
                   (wav-bytes (unwind-protect
                                  (copy-wav-pointer wav-pointer wav-length)
                                (vv-wav-free wav-pointer))))
              (values result wav-bytes wav-length))
            (values result nil 0))))))

(defmethod voicevox-synthesizer-synthesis ((self voicevox-class) audio-query-json style-id
                                           &optional options)
  (with-foreign-string (c-json audio-query-json)
    (call-wav-output-function #'vv-synthesizer-synthesis
                              (synthesizer-pointer self)
                              c-json
                              style-id
                              (or options (vv-make-default-synthesis-options)))))

(defmethod voicevox-synthesizer-tts ((self voicevox-class) text style-id &optional options)
  (with-foreign-string (c-text text)
    (call-wav-output-function #'vv-synthesizer-tts
                              (synthesizer-pointer self)
                              c-text
                              style-id
                              (or options (vv-make-default-tts-options)))))

(defmethod voicevox-synthesizer-tts-from-kana ((self voicevox-class) kana style-id &optional options)
  (with-foreign-string (c-kana kana)
    (call-wav-output-function #'vv-synthesizer-tts-from-kana
                              (synthesizer-pointer self)
                              c-kana
                              style-id
                              (or options (vv-make-default-tts-options)))))

(defmethod voicevox-synthesizer-create-sing-frame-audio-query ((self voicevox-class) score-json style-id)
  (with-foreign-string (c-score score-json)
    (call-json-output-function #'vv-synthesizer-create-sing-frame-audio-query
                               (synthesizer-pointer self)
                               c-score
                               style-id)))

(defmethod voicevox-synthesizer-create-sing-frame-f0 ((self voicevox-class) score-json frame-audio-query-json style-id)
  (with-foreign-string (c-score score-json)
    (with-foreign-string (c-query frame-audio-query-json)
      (call-json-output-function #'vv-synthesizer-create-sing-frame-f0
                                 (synthesizer-pointer self)
                                 c-score
                                 c-query
                                 style-id))))

(defmethod voicevox-synthesizer-create-sing-frame-volume ((self voicevox-class) score-json frame-audio-query-json style-id)
  (with-foreign-string (c-score score-json)
    (with-foreign-string (c-query frame-audio-query-json)
      (call-json-output-function #'vv-synthesizer-create-sing-frame-volume
                                 (synthesizer-pointer self)
                                 c-score
                                 c-query
                                 style-id))))

(defmethod voicevox-synthesizer-frame-synthesis ((self voicevox-class) frame-audio-query-json style-id)
  (with-foreign-string (c-json frame-audio-query-json)
    (call-wav-output-function #'vv-synthesizer-frame-synthesis
                              (synthesizer-pointer self)
                              c-json
                              style-id)))

(defclass voice-model-file-class ()
  ((voice-model-file-ptr
    :accessor voice-model-file-ptr
    :initform (foreign-alloc '(:pointer (:struct voicevox-voice-model-file))))))

(defmethod voice-model-file-open ((self voice-model-file-class) path)
  (with-foreign-string (c-path path)
    (vv-voice-model-file-open c-path (voice-model-file-ptr self))))

(defun voice-model-file-pointer (voice-model-file)
  (pointer-value (voice-model-file-ptr voice-model-file)
                 '(:pointer (:struct voicevox-voice-model-file))))

(defmethod voice-model-file-id ((self voice-model-file-class))
  (with-foreign-object (model-id '(:array :uint8 16))
    (vv-voice-model-file-id (voice-model-file-pointer self) model-id)
    (let ((result (make-array 16 :element-type '(unsigned-byte 8))))
      (dotimes (i 16 result)
        (setf (aref result i) (mem-aref model-id :uint8 i))))))

(defmethod voice-model-file-create-metas-json ((self voice-model-file-class))
  (json-pointer-to-string-and-free
   (vv-voice-model-file-create-metas-json (voice-model-file-pointer self))))

(defmethod voice-model-file-delete ((self voice-model-file-class))
  (vv-voice-model-file-delete (voice-model-file-pointer self)))

(defmethod voice-model-file-close ((self voice-model-file-class))
  (unless (null-pointer-p (voice-model-file-ptr self))
    (voice-model-file-delete self)
    (foreign-free (voice-model-file-ptr self))
    (setf (slot-value self 'voice-model-file-ptr) (null-pointer))))

(defclass user-dict-class ()
  ((user-dict-ptr :accessor user-dict-ptr :initform (null-pointer))))

(defmethod user-dict-new ((self user-dict-class))
  (setf (user-dict-ptr self) (vv-user-dict-new)))

(defmethod user-dict-load ((self user-dict-class) path)
  (with-foreign-string (c-path path)
    (vv-user-dict-load (user-dict-ptr self) c-path)))

(defmethod user-dict-add-word ((self user-dict-class) word)
  (with-foreign-object (output-word-uuid '(:array :uint8 16))
    (let ((result (vv-user-dict-add-word (user-dict-ptr self) word output-word-uuid))
          (uuid (make-array 16 :element-type '(unsigned-byte 8))))
      (dotimes (i 16)
        (setf (aref uuid i) (mem-aref output-word-uuid :uint8 i)))
      (values result uuid))))

(defmethod user-dict-update-word ((self user-dict-class) word-uuid word)
  (vv-user-dict-update-word (user-dict-ptr self) word-uuid word))

(defmethod user-dict-remove-word ((self user-dict-class) word-uuid)
  (vv-user-dict-remove-word (user-dict-ptr self) word-uuid))

(defmethod user-dict-to-json ((self user-dict-class))
  (call-json-output-function #'vv-user-dict-to-json (user-dict-ptr self)))

(defmethod user-dict-import ((self user-dict-class) other-dict)
  (vv-user-dict-import (user-dict-ptr self) (user-dict-ptr other-dict)))

(defmethod user-dict-save ((self user-dict-class) path)
  (with-foreign-string (c-path path)
    (vv-user-dict-save (user-dict-ptr self) c-path)))

(defmethod user-dict-delete ((self user-dict-class))
  (vv-user-dict-delete (user-dict-ptr self))
  (setf (user-dict-ptr self) (null-pointer)))

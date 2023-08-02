(in-package :cl-user)
(defpackage cl-unofficial-voicevox-core-wrapper
  (:use :cl :cffi)
  (:nicknames :unofficial-vv-core-wrapper)
  (:import-from
   :cl-unofficial-voicevox-core-wrapper.types
   :uint32
   :uint16
   :voicevox-result-code-type
   :voicevox-acceleration-mode-type
   :voicevox-user-dict-word-type-for-enum
   :voicevox-style-id-type)
  (:export
   :open-jtalk-rc-class
   #:open-jtalk-rc-new
   :synthesizer-class
   #:synthesizer-delete
   #:synthesizer-initialize
   #:synthesizer-is-gpu-mode
   #:synthesizer-audio-query
   #:synthesizer-synthesis
   #:synthesizer-tts
   :voicevox-result-code-type
   :voicevox-acceleration-mode-type
   :voicevox-version
   :user-dict-class
   #:user-dict-to-json
   #:user-dict-load-library
   #:load-library
   #:close-library))
(in-package :cl-unofficial-voicevox-core-wrapper)

(cffi:defctype c-style-id-type :uint32)
(cffi:defctype c-model-id-type (:pointer :char))

(cffi:defcenum voicevox-result-code
    (:voicevox-result-ok 0)
  (:voicevox-result-not-loaded-openjtalk-dict-error 1)
  (:voicevox-result-load-model-error 2)
  (:voicevox-result-get-supported-device-error 3)
  (:voicevox-result-gpu-support-error 4)
  (:voicevox-result-load-metas-error 5)
  (:voicevox-result-invalid-style-id-error 6)
  (:voicevox-result-invalid-model-index-error 7)
  (:voicevox-result-inference-error 8)
  (:voicevox-result-extract-full-context-label-error 11)
  (:voicevox-result-invalid-utf8-input-error 12)
  (:voicevox-result-parse-kana-error 13)
  (:voicevox-result-invalid-audio-query-error 14)
  (:voicevox-result-invalid-accent-phrase-error 15)
  (:voicevox-open-file-error 16)
  (:voicevox-vvm-model-read-error 17)
  (:voicevox-already-loaded-model-error 18)
  (:voicevox-unloaded-model-error 19)
  (:voicevox-load-user-dict-error 20)
  (:voicevox-save-user-dict-error 21)
  (:voicevox-unknown-user-dict-word-error 22)
  (:voicevox-use-user-dict-error 23)
  (:voicevox-invalid-user-dict-word-error 24)
  (:voicevox-result-invalid-uuid-error 25))

(cffi:defcenum voicevox-acceleration-mode-enum
  (:voicevox-acceleration-mode-auto 0)
  (:voicevox-acceleration-mode-cpu 1)
  (:voicevox-acceleration-mode-gpu 2))

(cffi:defcenum voicevox-user-dict-word-enum
  (:voicevox-user-dict-word-type-proper-noun 0)
  (:voicevox-user-dict-word-type-common-noun 1)
  (:voicevox-user-dict-word-type-verb 2)
  (:voicevox-user-dict-word-type-adjective 3)
  (:voicevox-user-dict-word-type-suffix 4))

(defun load-library (library)
  (cffi:load-foreign-library library))

(defun close-library (path)
  (cffi:close-foreign-library path))

(defun make-array-from-pointer (target length pointer-type value-type)
  (let ((result (make-array length)))
    (loop for i from 0 below length
          do
             (let ((tmp (cffi:mem-aref (cffi:mem-aref target pointer-type)
                                       value-type
                                       i)))
               (setf (aref result i) tmp)))
    result))


(defcfun ("voicevox_error_result_to_message" vv-error-result-to-message) :string
  (result-code voicevox-result-code))

(cffi:defcfun ("voicevox_json_free" vv-json-free) :void
  (json (:pointer :char)))

(cffi:defcfun ("voicevox_wav_free" vv-wav-free) :void
  (wav (:pointer :uint8)))


(declaim (inline get-result-from-code))
(defun get-result-from-code (code)
  (cffi:foreign-enum-keyword 'voicevox-result-code code))

(cffi:defcstruct voicevox-initialize-options
    (acceleration_mode voicevox-acceleration-mode-enum)
  (cpu_num_threads :uint16)
  (load_all_models :bool))

(cffi:defcstruct voicevox-audio-query-options
    (kana :int))

(cffi:defcstruct voicevox-accent-phrase-options
  (kana :int))

(cffi:defcstruct voicevox-synthesis-options
  (enable-interrogative-upspeak :int))


(cffi:defcstruct voicevox-tts-options
  (kana :bool)
  (enable-interrogative-upspeak :bool))

(cffi:defcstruct voicevox-user-dict-word
  (surface (:pointer :char))
  (pronunciation (:pointer :char))
  (accent-type :uintptr)
  (word-type voicevox-user-dict-word-enum)
  (priority :uint32))

(cffi:defcstruct open-jtalk-rc)
(cffi:defcstruct voicevox-synthesizer)
(cffi:defcstruct voicevox-user-dict)
(cffi:defcstruct voicevox-voice-model)

(cffi:defcvar ("voicevox_default_initialize_options" vv-default-initialize-options)
    (:struct voicevox-initialize-options))
(cffi:defcvar ("voicevox_version" voicevox-version :read-only t) :string)
(cffi:defcvar ("voicevox_default_audio_query_options" vv-default-audio-query-options)
    (:struct voicevox-audio-query-options))
(cffi:defcvar ("voicevox_default_accent_phrase_options" vv-default-accent-phrase-options)
  (:struct voicevox-accent-phrase-options))
(cffi:defcvar ("voicevox_default_synthesis_options" vv-default-synthesis-options)
    (:struct voicevox-synthesis-options))
(cffi:defcvar ("voicevox_default_tts_options" vv-default-tts-options)
    (:struct voicevox-tts-options))

(cffi:defcfun ("voicevox_open_jtalk_rc_new" vv-open-jtalk-rc-new) :int
  (open-jtalk-dic-dir (:pointer :char))
  (out-open-jtalk (:pointer (:pointer (:struct open-jtalk-rc)))))

(cffi:defcfun ("voicevox_open_jtalk_rc_use_user_dict" vv-open-jtalk-rc-use-user-dict) :int
  (open-jtalk (:pointer (:pointer (:struct open-jtalk-rc)))))

(cffi:defcfun ("voicevox_open_jtalk_rc_delete" vv-open-jtalk-rc-delete) :void
  (open-jtalk (:pointer (:struct open-jtalk-rc))))

(cffi:defcfun ("voicevox_voice_model_new_from_path" vv-voice-model-new-from-path) :int
  (path (:pointer (:char)))
  (out-model (:pointer (:pointer (:struct voicevox-voice-model)))))

(cffi:defcfun ("voicevox_voice_model_id" vv-voice-model-id) :string
  (model (:pointer (:struct voicevox-voice-model))))

(cffi:defcfun ("voicevox_voice_model_get_metas_json" vv-voice-model-get-metas-json) :string
  (model (:pointer (:struct voicevox-voice-model))))

(cffi:defcfun ("voicevox_voice_model_delete" vv-voice-model-delete) :void
  (model (:pointer (:struct voicevox-voice-model))))

(cffi:defcfun ("voicevox_synthesizer_new_with_initialize" vv-synthesizer-new-with-initialize) :int
  (open-jtalk (:pointer (:struct open-jtalk-rc)))
  (options (:struct voicevox-initialize-options))
  (out-synthesizer (:pointer (:pointer (:struct voicevox-synthesizer)))))

(cffi:defcfun ("voicevox_synthesizer_delete" vv-synthesizer-delete) :void
  (synthesizer (:pointer (:struct voicevox-synthesizer))))

(cffi:defcfun ("voicevox_synthesizer_load_voice_model" vv-synthesizer-load-voice-model) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (model (:pointer (:struct voicevox-voice-model))))

(cffi:defcfun ("voicevox_synthesizer_unload_voice_modl" vv-synthesizer-unload-voice-model) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (model-id c-model-id-type))

(cffi:defcfun ("voicevox_synthesizer_is_gpu_mode" vv-synthesizer-is-gpu-mode) :bool
  (synthesizer (:pointer (:struct voicevox-synthesizer))))

(cffi:defcfun ("voicevox_synthesizer_is_loaded_voice_model" vv-synthesizer-is-load-voice-model) :bool
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (model-id c-model-id-type))

(cffi:defcfun ("voicevox_synthesizer_get_metas_json" vv-synthesizer-get-metas-json) :string
  (synthesizer (:pointer (:struct voicevox-synthesizer))))

(cffi:defcfun ("voicevox_create_supported_devices_json" vv-create-supported-devices-json) :int
  (output-supported-devices-json (:pointer (:pointer :char))))

(cffi:defcfun ("voicevox_synthesizer_audio_query" vv-synthesizer-audio-query) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (text (:pointer :char))
  (style-id c-style-id-type)
  (options (:struct voicevox-audio-query-options))
  (output-audio-query-json (:pointer (:pointer :char))))

(cffi:defcfun ("voicevox_synthesizer_create_accent_phrase" vv-synthesizer-crate-accent-phrase) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (text (:pointer :char))
  (style-id c-style-id-type)
  (options (:struct voicevox-accent-phrase-options))
  (output-accent-phrase-json (:pointer (:pointer :char))))

(cffi:defcfun ("voicevox_synthesizer_replace_mora_data" vv-synthesizer-replace-mora-data) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (accent-phrase-json (:pointer :char))
  (style-id c-style-id-type)
  (output-accent-phrase-json (:pointer (:pointer :char))))

(cffi:defcfun ("voicevox_synthesizer_replace_phoneme_length" vv-synthesizer-replace-phoneme-length) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (accent-phrase-json (:pointer :char))
  (style-id c-style-id-type)
  (output-accent-phrase-json (:pointer (:pointer :char))))

(cffi:defcfun ("voicevox_synthesizer_replace_mora_pitch" vv-synthesizer-replace-mora-pitch) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (accent-phrase-json (:pointer :char))
  (style-id c-style-id-type)
  (output-accent-phrase-json (:pointer (:pointer :char))))

(cffi:defcfun ("voicevox_synthesizer_synthesis" vv-synthesizer-synthesis) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (audio-query-json (:pointer :char))
  (style-id c-style-id-type)
  (options (:struct voicevox-synthesis-options))
  (output-wav-length (:pointer :uintptr))
  (output-wav (:pointer (:pointer :uint8))))

(cffi:defcfun ("voicevox_synthesizer_tts" vv-synthesizer-tts) :int
  (synthesizer (:pointer (:struct voicevox-synthesizer)))
  (text (:pointer :char))
  (style-id c-style-id-type)
  (options (:struct voicevox-tts-options))
  (output-wav-length (:pointer :uintptr))
  (output-wav (:pointer (:pointer :uint8))))

(cffi:defcfun ("voicevox_user_dict_delete" vv-user-dict-delete) :void
  (user-dict (:pointer (:struct voicevox-user-dict))))

(cffi:defcfun ("voicevox_user_dict_word_make" vv-user-dict-word-make) (:struct voicevox-user-dict-word)
  (surface (:pointer :char))
  (pronunciation (:pointer :char)))

(cffi:defcfun ("voicevox_user_dict_new" vv-user-dict-new) (:pointer (:struct voicevox-user-dict)))

(cffi:defcfun ("voicevox_user_dict_load" vv-user-dict-load) :int
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (dict-path (:pointer :char)))

(cffi:defcfun ("voicevox_user_dict_add_word" vv-user-dict-add-word) :int
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (word (:pointer (:struct voicevox-user-dict-word)))
  (output-word-uuid (:pointer :uint8)))

(cffi:defcfun ("voicevox_user_dict_update_word" vv-user-dict-update-word) :int
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (word-uuid (:pointer :uint8))
  (word (:pointer (:struct voicevox-user-dict-word))))

(cffi:defcfun ("voicevox_user_dict_remove_word" vv-user-dict-remove-word) :int
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (word-uuid (:pointer :uint8)))

(cffi:defcfun ("voicevox_user_dict_to_json" vv-user-dict-to-json) :int
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (output-json (:pointer (:pointer :char))))

(cffi:defcfun ("voicevox_user_dict_import" vv-user-dict-import) :int
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (other-dict (:pointer (:struct voicevox-user-dict))))

(cffi:defcfun ("voicevox_user_dict_save" vv-user-dict-save) :int
  (user-dict (:pointer (:struct voicevox-user-dict)))
  (path (:pointer :char)))

(defclass open-jtalk-rc-class ()
  ((open-jtalk-rc-ptr :accessor open-jtalk-rc-ptr :initform (cffi:foreign-alloc '(:pointer (:struct open-jtalk-rc))))))

(defmethod open-jtalk-rc-new ((self open-jtalk-rc-class)
                              open-jtalk-dic-dir)
  (cffi:with-foreign-string (c-open-jtalk-dic-dir open-jtalk-dic-dir)
    (get-result-from-code
     (vv-open-jtalk-rc-new c-open-jtalk-dic-dir
                           (slot-value self 'open-jtalk-rc-ptr)))))

(defmethod open-jtalk-rc-delete ((self open-jtalk-rc-class))
  (vv-open-jtalk-rc-delete (cffi:mem-ref
                            (slot-value self 'open-jtalk-rc-ptr)
                            '(:pointer (:struct open-jtalk-rc)))))

(defmethod open-jtalk-rc-close ((self open-jtalk-rc-class))
  (unless (cffi:null-pointer-p (slot-value self 'open-jtalk-rc-ptr))
    (open-jtalk-rc-delete self))
  (cffi:foreign-free (slot-value self 'open-jtalk-rc-ptr)))

(defclass voice-model-class ()
  ((voice-model :accessor voice-model :initform (cffi:foreign-alloc
                                                 '(:pointer (:struct voicevox-voice-model))))))
(defmethod voice-model-new-from-path ((self voice-model-class)
                                     path)
  (cffi:with-foreign-string (c-path path)
      (get-result-from-code
       (vv-voice-model-new-from-path
        c-path
        (slot-value self 'voice-model)))))

(defmethod voice-model-id ((self voice-model-class))
  (vv-voice-model-id (cffi:mem-ref (slot-value self 'voice-model)
                                   '(:pointer (:struct voicevox-voice-model)))))

(defmethod voice-model-get-metas-json ((self voice-model-class))
  (vv-voice-model-get-metas-json (cffi:mem-ref (slot-value self 'voice-model)
                                               '(:pointer (:struct voicevox-voice-model)))))

(defmethod voice-model-delete ((self voice-model-class))
  (vv-voice-model-delete (cffi:mem-ref (slot-value self 'voice-model)
                                       '(:pointer (:struct voicevox-voice-model)))))

(defmethod voice-model-close ((self voice-model-class))
  (unless (cffi:null-pointer-p (slot-value self 'voice-model))
    (voice-model-delete self))
  (cffi:foreign-free (slot-value self 'voice-model)))

(defclass synthesizer-class ()
  ((synthesizer :accessor synthesizer :initform (cffi:foreign-alloc
                                                 '(:pointer (:struct voicevox-synthesizer))))))

(defmethod synthesizer-delete ((self synthesizer-class))
  (vv-synthesizer-delete (cffi:mem-ref (slot-value self 'synthesizer)
                                       '(:pointer (:struct voicevox-synthesizer)))))

(defmethod synthesizer-close ((self synthesizer-class))
  (unless (cffi:null-pointer-p (slot-value self 'synthesizer))
    (synthesizer-delete self))
  (cffi:foreign-free (slot-value self 'synthesizer)))

(defmethod synthesizer-initialize ((self synthesizer-class)
                                   acceleration-mode
                                   cpu-num-threads
                                   load-all-models
                                   open-jtalk-rc-instance)
  (cffi:with-foreign-object (options '(:struct voicevox-initialize-options))
    (setf (cffi:foreign-slot-value options '(:struct voicevox-initialize-options) 'acceleration_mode) acceleration-mode
          (cffi:foreign-slot-value options '(:struct voicevox-initialize-options) 'cpu_num_threads) cpu-num-threads
          (cffi:foreign-slot-value options '(:struct voicevox-initialize-options) 'load_all_models) load-all-models)
    (let ((result-status
            (get-result-from-code
             (vv-synthesizer-new-with-initialize
              (cffi:mem-ref
               (slot-value open-jtalk-rc-instance 'open-jtalk-rc-ptr)
               '(:pointer (:struct open-jtalk-rc)))
              (cffi:mem-ref options '(:struct voicevox-initialize-options))
              (slot-value self 'synthesizer)))))
      result-status)))

(defmethod synthesizer-load-voice-model ((self synthesizer-class)
                                         voicevox-model-instance)
  (get-result-from-code
   (vv-synthesizer-load-voice-model
    (cffi:mem-ref (slot-value self 'synthesizer)
                  '(:pointer (:struct voicevox-synthesizer)))
    (cffi:mem-ref (slot-value voicevox-model-instance 'voice-model)
                  '(:pointer (:struct voicevox-voice-model))))))

(defmethod synthesizer-unload-voice-model ((self synthesizer-class)
                                           model-id)
  (cffi:with-foreign-string (c-model-id model-id)
    (vv-synthesizer-unload-voice-model
     (cffi:mem-ref (slot-value self 'synthesizer) '(:pointer (:struct voicevox-synthesizer)))
     c-model-id)))

(defmethod synthesizer-is-gpu-mode ((self synthesizer-class))
  (vv-synthesizer-is-gpu-mode (cffi:mem-ref (slot-value self 'synthesizer)
                                            '(:pointer (:struct voicevox-synthesizer)))))

(defmethod synthesizer-is-loaded-voice-model ((self synthesizer-class)
                                              model-id)
  (cffi:with-foreign-string (c-model-id model-id)
    (vv-synthesizer-is-load-voice-model
     (cffi:mem-ref (slot-value self 'synthesizer) '(:pointer (:struct voicevox-synthesizer)))
     c-model-id)))

(defmethod synthesizer-get-metas-json ((self synthesizer-class))
  (vv-synthesizer-get-metas-json (cffi:mem-ref (slot-value self 'synthesizer) '(:pointer (:struct voicevox-synthesizer)))))

(defmethod create-supported-devices-json ()
  (cffi:with-foreign-object (output-supported-devices-json '(:pointer :char))
    (let ((result-status
            (get-result-from-code
             vv-create-supported-devices-json output-supported-devices-json)))
      (if (eq result-status :voicevox-result-ok)
          (let ((output-supported-devices-json-lisp-string
                  (cffi:foreign-string-to-lisp
                   (cffi:mem-aref output-supported-devices-json '(:pointer :char)))))
            (list :result-status result-status :supported-devices-json output-supported-devices-json-lisp-string))
          (list :result-status result-status)))))

(defmethod synthesizer-audio-query ((self synthesizer-class)
                                    text
                                    style-id
                                    kana)
  (cffi:with-foreign-objects ((options '(:struct voicevox-audio-query-options))
                              (output-audio-query-json '(:pointer :char)))
    (setf (cffi:foreign-slot-value options '(:struct voicevox-audio-query-options) 'kana)
          (if kana 1 0))
    (cffi:with-foreign-string (c-text text)
      (let ((result-status
              (get-result-from-code
               (vv-synthesizer-audio-query
                (cffi:mem-ref
                 (slot-value self 'synthesizer)
                 '(:pointer (:struct voicevox-synthesizer)))
                c-text
                style-id
                (cffi:mem-ref options '(:struct voicevox-audio-query-options))
                output-audio-query-json))))
        (if (eq result-status :voicevox-result-ok)
            (let ((output-audio-query-json-lisp
                    (cffi:foreign-string-to-lisp (cffi:mem-aref output-audio-query-json
                                                                '(:pointer :char)))))
              (list :result-status result-status :audio-query-json output-audio-query-json-lisp))
            (list :result-status result-status))))))

(defmethod synthesizer-synthesis ((self synthesizer-class)
                      audio-query-json
                      style-id
                      enable-interrogative-upspeak)
  (cffi:with-foreign-objects ((output-wav-length :uintptr)
                              (output-wav '(:pointer :uint8))
                              (options '(:struct voicevox-synthesis-options)))
    (setf (cffi:foreign-slot-value options '(:struct voicevox-synthesis-options) 'enable-interrogative-upspeak)
          (if enable-interrogative-upspeak 1 0))
    (cffi:with-foreign-string (c-audio-query-json audio-query-json)
      (let ((result-status
              (get-result-from-code
               (vv-synthesizer-synthesis
                (cffi:mem-ref (slot-value self 'synthesizer)
                              '(:pointer (:struct voicevox-synthesizer)))
                c-audio-query-json
                style-id
                (cffi:mem-ref options '(:struct voicevox-synthesis-options))
                output-wav-length
                output-wav))))
        (if (eq result-status :voicevox-result-ok)
            (let* ((wav-length-unref (cffi:mem-ref output-wav-length :uintptr))
                   (wav-lisp-array (make-array-from-pointer
                                    output-wav
                                    wav-length-unref
                                    '(:pointer :uint8)
                                    :uint8)))
              (list :result-status result-status :wav-length wav-length-unref :wav-bytes wav-lisp-array))
            (list :result-status result-status))))))

(defmethod synthesizer-create-accent-phrase ((self synthesizer-class)
                                             text
                                             style-id
                                             kana)
  (cffi:with-foreign-objects ((output-accent-phrase-json '(:pointer :char))
                              (options '(:struct voicevox-accent-phrase-options)))
    (setf (cffi:foreign-slot-value options '(:struct voicevox-accent-phrase-options)
                                   'kana)
          (if kana 1 0))
    (cffi:with-foreign-string (c-text text)
      (let ((result-status
              (get-result-from-code
               (vv-synthesizer-crate-accent-phrase
                (cffi:mem-ref (slot-value self 'synthesizer) '(:pointer (:struct voicevox-synthesizer)))
                c-text
                (cffi:mem-ref options '(:struct voicevox-accent-phrase-options))
                output-accent-phrase-json))))
        (if (eq result-status :voicevox-result-ok)
            (let ((output-accent-phrase-json-lisp-string
                    (cffi:foreign-string-to-lisp (cffi:mem-aref output-accent-phrase-json '(:pointer :char)))))
              (list :result-status result-status :accent-phrase-json output-accent-phrase-json-lisp-string))
            (list :result-status result-status))))))

(defmethod synthesizer-replace-mora-data ((self synthesizer-class)
                                          accent-phrase-json
                                          style-id)
  (cffi:with-foreign-object (output-accent-phrase-json '(:pointer :char))
    (cffi:with-foreign-string (c-accent-phrase-json accent-phrase-json)
      (let ((result-status
              (get-result-from-code
               (vv-synthesizer-replace-mora-data
                (cffi:mem-ref (slot-value self 'synthesizer) '(:pointer (:struct voicevox-synthesizer)))
                c-accent-phrase-json
                style-id
                output-accent-phrase-json))))
        (if (eq result-status :voicevox-result-ok)
            (let ((output-accent-phrase-json-lisp-string
                    (cffi:foreign-string-to-lisp (cffi:mem-aref output-accent-phrase-json '(:pointer :char)))))
              (list :result-status result-status :accent-phrase-json output-accent-phrase-json-lisp-string))
            (list :result-status result-status))))))


(defmethod synthesizer-replace-phoneme-length ((self synthesizer-class)
                                               accent-phrase-json
                                               style-id)
  (cffi:with-foreign-object (output-accent-phrase-json '(:pointer :char))
    (cffi:with-foreign-string (c-accent-phrase-json accent-phrase-json)
      (let ((result-status
              (get-result-from-code
               (vv-synthesizer-replace-phoneme-length
                (cffi:mem-ref (slot-value self 'synthesizer) '(:pointer (:struct voicevox-synthesizer)))
                c-accent-phrase-json
                style-id
                output-accent-phrase-json))))
        (if (eq result-status :voicevox-result-ok)
            (let ((output-accent-phrase-json-lisp-string
                    (cffi:foreign-string-to-lisp (cffi:mem-aref output-accent-phrase-json '(:pointer :char)))))
              (list :result-status result-status :accent-phrase-json output-accent-phrase-json-lisp-string))
            (list :result-status result-status))))))

(defmethod synthesizer-replace-mora-pitch ((self synthesizer-class)
                                           accent-phrase-json
                                           style-id)
  (cffi:with-foreign-object (output-accent-phrase-json '(:pointer :char))
    (cffi:with-foreign-string (c-accent-phrase-json accent-phrase-json)
      (let ((result-status
              (get-result-from-code
               (vv-synthesizer-replace-mora-pitch
                (cffi:mem-ref (slot-value self 'synthesizer) '(:pointer (:struct voicevox-synthesizer)))
                c-accent-phrase-json
                style-id
                output-accent-phrase-json))))
        (if (eql result-status :voicevox-result-ok)
            (let ((output-accent-phrase-json-lisp-string
                    (cffi:foreign-string-to-lisp (cffi:mem-aref output-accent-phrase-json '(:pointer :char)))))
              (list :result-status result-status :accent-phrase-json output-accent-phrase-json-lisp-string))
            (list :result-status result-status))))))

(defmethod synthesizer-tts ((self synthesizer-class)
                            text
                            style-id
                            kana
                            enable-interrogative-upspeak)
  (cffi:with-foreign-objects ((output-wav-length :uintptr)
                              (output-wav '(:pointer :uint8))
                              (options '(:struct voicevox-tts-options)))
    (setf (cffi:foreign-slot-value options '(:struct voicevox-tts-options) 'kana) kana
          (cffi:foreign-slot-value options '(:struct voicevox-tts-options) 'enable-interrogative-upspeak) enable-interrogative-upspeak)
    (cffi:with-foreign-string (c-text text)
      (let ((result-status
              (get-result-from-code
               (vv-synthesizer-tts
                (cffi:mem-ref (slot-value self 'synthesizer)
                              '(:pointer (:strcut voicevox-synthesizer)))
                c-text
                style-id
                (cffi:mem-ref options '(:pointer (:struct voicevox-tts-options)))
                output-wav-length
                output-wav))))
        (if (eq result-status :voicevox-result-ok)
            (let* ((wav-length-unref (cffi:mem-ref output-wav-length :uintptr))
                   (wav-lisp-array (make-array-from-pointer
                                    output-wav
                                    wav-length-unref
                                    '(:pointer :uint8)
                                    :uint8)))
              (list :result-status result-status :wav-length wav-length-unref :wav-bytes wav-lisp-array))
            (list :result-status result-status))))))

(defclass user-dict-class ()
  ((user-dict :accessor user-dict :initform (let ((user-dict-alloc
                                                    (cffi:foreign-alloc '(:pointer (:struct voicevox-user-dict)))))
                                              (setf (cffi:mem-ref user-dict-alloc
                                                                  '(:pointer (:struct voicevox-user-dict)))
                                                    (vv-user-dict-new))
                                              user-dict-alloc))))

(defmethod user-dict-delete ((self user-dict-class))
  (vv-user-dict-delete (cffi:mem-ref (slot-value self 'user-dict)
                                     '(:pointer (:struct voicevox-user-dict)))))

(defmethod user-dict-close ((self user-dict-class))
  (unless (cffi:null-pointer-p (cffi:mem-ref (slot-value self 'user-dict)
                                             '(:pointer (:struct voicevox-user-dict))))
    (user-dict-delete self))
  (cffi:foreign-free (slot-value self 'user-dict)))

(defmethod user-dict-new ((self user-dict-class))
  (setf (cffi:mem-ref (slot-value self 'user-dict)
                      '(:pointer (:struct voicevox-user-dict)))
        (vv-user-dict-new)))

(defmethod user-dict-load ((self user-dict-class)
                           dict-path)
  (cffi:with-foreign-string (c-dict-path dict-path)
    (get-result-from-code
     (vv-user-dict-load
      (cffi:mem-ref (slot-value self 'user-dict) '(:pointer (:struct voicevox-user-dict)))
      c-dict-path))))

(defmethod user-dict-add-word ((self user-dict-class)
                               surface
                               pronunciation)
  (cffi:with-foreign-objects ((word '(:struct voicevox-user-dict-word))
                              (output-word-uuid '(:pointer :uint8) 16))
    (cffi:with-foreign-strings ((c-surface surface)
                                (c-pronunciation pronunciation))
      (setf
       (cffi:mem-ref word '(:struct voicevox-user-dict-word))
       (vv-user-dict-word-make c-surface c-pronunciation))
      (let ((result-status
              (get-result-from-code
               (vv-user-dict-add-word
                (cffi:mem-ref (slot-value self 'user-dict) '(:pointer (:struct voicevox-user-dict)))
                word
                output-word-uuid))))
        (if (eq result-status :voicevox-result-ok)
            (let ((word-uuid-array
                    (make-array-from-pointer
                     output-word-uuid
                     16
                     '(:pointer :uint8)
                     :uint8)))
              (list :result-status result-status
                    :word-uuid word-uuid-array))
            (list :result-status result-status))))))

(defmethod user-dict-update-word ((self user-dict-class)
                                               word-uuid
                                               surface
                                               pronunciation)
  (unless (/= (length array) 16)
    :must-be-word-uuid-length-is-16)
  (cffi:with-foreign-objects ((c-word-uuid '(:pointer :uint8) 16)
                              (word '(:struct voicevox-user-dict-word)))
    (cffi:with-foreign-strings ((c-surface surface)
                                (c-pronunciation pronunciation))
      (loop for i from 0 below 16
            do (setf (cffi:mem-aref c-word-uuid '(:pointer :uint8) i) (aref word-uuid i)))
      (setf (cffi:mem-ref word '(:strcut voicevox-user-dict-word))
            (vv-user-dict-word-make c-surface c-pronunciation))
      (get-result-from-code
       (vv-user-dict-update-word
        (cffi:mem-ref (slot-value self 'user-dict) '(:pointer (:struct voicevox-user-dict)))
        c-word-uuid
        word)))))

(defmethod user-dict-to-json ((self user-dict-class))
  (cffi:with-foreign-object (output-json '(:pointer :char))
    (let ((result-status
            (get-result-from-code
             (vv-user-dict-to-json
              (cffi:mem-ref (slot-value self 'user-dict) '(:pointer (:struct voicevox-user-dict)))
              output-json))))
      (if (equal result-status :voicevox-result-ok)
          (list :result-status result-status
                :user-dict-json (cffi:foreign-string-to-lisp (cffi:mem-aref output-json '(:pointer :char))))
          (list :result-status result-status)))))

(defmethod user-dict-import ((self user-dict-class)
                                          other-dict-instance)
  (get-result-from-code
           (vv-user-dict-import
            (cffi:mem-ref (slot-value self 'user-dict) '(:pointer (:struct voicevox-user-dict)))
            (cffi:mem-ref (slot-value other-dict-instance 'user-dict) '(:pointer (:struct voicevox-user-dict))))))


(defmethod user-dict-save ((self user-dict-class)
                                        path)
  (cffi:with-foreign-string (c-path path)
    (get-result-from-code (vv-user-dict-save
                           (cffi:mem-ref (slot-value self 'user-dict) '(:pointer (:struct voicevox-user-dict)))
                           c-path))))



;; (cffi:defcfun ("voicevox_initialize" vv-initialize) :int
;;   "initialize for voicevox core"
;;   (options (:struct voicevox-initialize-options)))
;; (declaim (ftype (function (&key (:acceleration-mode voicevox-acceleration-mode-type)
;;                                 (:cpu-num-threads uint16)
;;                                 (:load-all-models boolean)
;;                                 (:open-jtalk-dict-dir string))
;;                           (values voicevox-result-code-type &optional))
;;                 initialize))
;; (defun initialize (&key acceleration-mode cpu-num-threads load-all-models open-jtalk-dict-dir)
;;   (cffi:with-foreign-objects ((options-ini '(:struct voicevox-initialize-options)))
;;     (cffi:with-foreign-strings ((open-jtalk-dict-dir-path-c open-jtalk-dict-dir))
;;       (setf (cffi:foreign-slot-value options-ini '(:struct voicevox-initialize-options) 'acceleration_mode) acceleration-mode
;;             (cffi:foreign-slot-value options-ini '(:struct voicevox-initialize-options) 'cpu_num_threads) cpu-num-threads
;;             (cffi:foreign-slot-value options-ini '(:struct voicevox-initialize-options) 'load_all_models) load-all-models
;;             (cffi:foreign-slot-value options-ini '(:struct voicevox-initialize-options) 'open_jtalk_dict_dir) open-jtalk-dict-dir-path-c)
;;       (get-result-from-code (vv-initialize (cffi:mem-ref options-ini '(:struct voicevox-initialize-options)))))))

;; (cffi:defcfun ("voicevox_is_gpu_mode" is-gpu-mode) :bool)
;; (cffi:defcfun ("voicevox_get_version" get-version) :string)
;; (cffi:defcfun ("voicevox_load_model" load-model) :int
;;   (speaker-id :uint32))
;; (cffi:defcfun ("voicevox_is_model_loaded" is-model-loaded) :bool
;;   (speaker-id :uint32))
;; (cffi:defcfun ("voicevox_finalize" finalize) :void)
;; (cffi:defcfun ("voicevox_get_metas_json" get-metas-json) :string)
;; (cffi:defcfun ("voicevox_get_supported_device_json" get-supported-device-json) :string)
;; (cffi:defcfun ("voicevox_wav_free" vv-wav-free) :void
;;   (wav (:pointer :uint8)))


;; (cffi:defcfun ("voicevox_audio_query_json_free" vv-audio-query-json-free) :void
;;   (audio-query-json (:pointer :char)))
;; (cffi:defcfun ("voicevox_audio_query" vv-audio-query) :int
;;   "Do audio query"
;;   (text (:pointer :char))
;;   (speaker-id :uint32)
;;   (options (:struct voicevox-audio-query-options))
;;   (output-audio-query-json (:pointer (:pointer :char))) ;; this pointer is **char
;;   )


;; (cffi:defcfun ("voicevox_tts" vv-tts) :int
;;   (text (:pointer :char))
;;   (speaker-id :uint32)
;;   (options (:struct voicevox-tts-options))
;;   (output-wav-length (:pointer :uintptr))
;;   (output-wav (:pointer (:pointer :char))))



;; (defun free-for-foreign (&rest targets)
;;   (dolist (x targets)
;;     (cffi:foreign-free x)))

;; (declaim (ftype (function (&key
;;                            (:text string)
;;                            (:kana boolean)
;;                            (:speaker-id uint32))
;;                           list)
;;                 audio-query))
;; (defun audio-query (&key text kana speaker-id)
;;   (cffi:with-foreign-objects ((audio-query-options '(:struct voicevox-audio-query-options))
;;                               (out-audio-query-json '(:pointer :char)))
;;     (cffi:with-foreign-strings ((text-c text))
;;       (setf (cffi:foreign-slot-value audio-query-options '(:struct voicevox-audio-query-options) 'kana) (if kana 1 0))
;;       (let ((result-status (get-result-from-code
;;                             (vv-audio-query
;;                              text-c
;;                              speaker-id
;;                              (cffi:mem-ref audio-query-options '(:struct voicevox-audio-query-options))
;;                              out-audio-query-json))))
;;         (if (equal result-status :voicevox-result-ok)
;;             (let ((result (cffi:foreign-string-to-lisp (cffi:mem-aref out-audio-query-json '(:pointer :char)))))
;;               (list :result-status result-status :audio-query result))
;;             (list :result-status result-status))))))

;; (declaim (ftype (function (&key
;;                            (:text string )
;;                            (:kana boolean)
;;                            (:enable-interrogative-upspeak boolean)
;;                            (:speaker-id uint32))
;;                           (values list &optional))
;;                 tts))
;; (defun tts (&key text kana enable-interrogative-upspeak speaker-id)
;;   (cffi:with-foreign-objects ((options-tts '(:struct voicevox-tts-options))
;;                               (wav-length :uintptr)
;;                               (out-wav-bytes (:pointer :uint8)))
;;     (cffi:with-foreign-strings ((text-c text))
;;       (setf
;;        (cffi:foreign-slot-value options-tts '(:struct voicevox-tts-options) 'kana) kana
;;        (cffi:foreign-slot-value options-tts '(:struct voicevox-tts-options) 'enable-interrogative-upspeak) enable-interrogative-upspeak)
;;       (let ((result-status
;;               (get-result-from-code
;;                (vv-tts
;;                 text-c
;;                 speaker-id
;;                 (cffi:mem-ref options-tts '(:struct voicevox-tts-options))
;;                 wav-length
;;                 out-wav-bytes
;;                 ))))
;;         (if (eq result-status :voicevox-result-ok)
;;             (let* ((wav-length-unref (cffi:mem-ref wav-length :uint))
;;                    (wav-bytes-array (make-array-from-pointer
;;                                      out-wav-bytes
;;                                      wav-length-unref
;;                                      (:pointer :uint8)
;;                                      :uint8)))
;;               (list :result-status result-status :wav-length wav-length-unref :wav-bytes wav-bytes-array))
;;             (list :result-status result-status))))))




;; (defcfun ("voicevox_predict_duration_data_free" vv-predict-duration-data-free) :void
;;   (predict-duration-data (:pointer :float)))
;; (defcfun ("voicevox_predict_duration" vv-predict-duration) :int
;;   (length :uintptr)
;;   (phoneme-vector (:pointer :int64))
;;   (speaker-id :uint32)
;;   (output-predict-durarion-data-length (:pointer :uintptr))
;;   (output-predict-durarion-data (:pointer (:pointer :float))))
;; (defun predict-duration (length phoneme-vector speaker-id)
;;   (cffi:with-foreign-objects ((arr :int64 length)
;;                               (output-predict-durarion-data-length :uintptr)
;;                               (output-predict-durarion-data (:pointer :float)))
;;     (loop for i from 0 below length
;;           do (setf (cffi:mem-aref arr :int64 i) (aref phoneme-vector i)))
;;     (let ((result-status (get-result-from-code (vv-predict-duration
;;                                                 length
;;                                                 arr
;;                                                 output-predict-durarion-data-length
;;                                                 output-predict-durarion-data))))
;;       (if (eq result-status :voicevox-result-ok)
;;           (let ((output-predict-durarion-data-length-unref (cffi:mem-ref output-predict-durarion-data-length :uintptr))
;;                 (output-predict-durarion-data-lisp-array (make-array-from-pointer
;;                                                           output-predict-durarion-data
;;                                                           output-predict-durarion-data-length-unref
;;                                                           (:pointer :float)
;;                                                           :float)))
;;             (list :result-status result-status
;;                   :predict-duration-data-length output-predict-durarion-data-length-unref
;;                   :predict-duration-data output-predict-durarion-data-lisp-array))
;;           (list :result-status result-status)))))

;; (defcfun ("voicevox_predict_intonation_data_free" vv-predict-intonation-data-free) :void
;;   (predict-intonation-data (:pointer :float)))
;; (defcfun ("voicevox_predict_intonation" vv-predict-intonation) :int
;;   (length :uintptr)
;;   (vowel-phoneme-vector (:pointer :int64))
;;   (consonant-phoneme-vector (:pointer :int64))
;;   (start-accent-vector (:pointer :int64))
;;   (end-accent-vector (:pointer :int64))
;;   (start-accent-phrase-vector (:pointer :int64))
;;   (end-accent-phrase-vector (:pointer :int64))
;;   (speaker-id :uint32)
;;   (output-predict-intonation-data-length :uintptr)
;;   (output-predict-intonation-data (:pointer (:pointer :float))))
;; (defun predict-intonation (length
;;                            vowel-phoneme-vector
;;                            consonant-phoneme-vector
;;                            start-accent-vector
;;                            end-accent-vector
;;                            start-accent-phrase-vector
;;                            end-accent-phrase-vector
;;                            speaker-id)
;;   (cffi:with-foreign-objects ((c-vowel-phoneme-vector :int64 length)
;;                               (c-consonant-phoneme-vector :int64 length)
;;                               (c-start-accent-vector :int64 length)
;;                               (c-end-accent-vector :int64 length)
;;                               (c-start-accent-phrase-vector :int64 length)
;;                               (c-end-accent-phrase-vector :int64 length)
;;                               (output-predict-intonation-data-length :uintptr)
;;                               (output-predict-intonation-data (:pointer :float)))
;;     (loop for i from 0 below length
;;           do (setf
;;               (cffi:mem-aref c-vowel-phoneme-vector :int64 i) (aref vowel-phoneme-vector i)
;;               (cffi:mem-aref c-consonant-phoneme-vector :int64 i) (aref consonant-phoneme-vector i)
;;               (cffi:mem-aref c-start-accent-vector :int64 i) (aref start-accent-vector i)
;;               (cffi:mem-aref c-end-accent-vector :int64 i) (aref end-accent-vector i)
;;               (cffi:mem-aref c-start-accent-phrase-vector :int64 i) (aref start-accent-phrase-vector i)
;;               (cffi:mem-aref c-end-accent-phrase-vector ':int64 i) (aref end-accent-phrase-vector i)))
;;     (let ((result-status
;;             (get-result-from-code
;;              (vv-predict-intonation
;;               length
;;               c-vowel-phoneme-vector
;;               c-consonant-phoneme-vector
;;               c-start-accent-vector
;;               c-end-accent-vector
;;               c-start-accent-phrase-vector
;;               c-end-accent-phrase-vector
;;               output-predict-intonation-data-length
;;               output-predict-intonation-data))))
;;       (if (eq result-status :voicevox-result-ok)
;;           (let* ((output-predict-intonation-data-length-unref (cffi:mem-ref output-predict-intonation-data-length :uintptr))
;;                  (predict-intonation-data-lisp-array (make-array-from-pointer
;;                                                       output-predict-intonation-data
;;                                                       output-predict-intonation-data-length-unref
;;                                                       (:pointer :float)
;;                                                       :float)))
;;             (list :result-status result-status
;;                   :predict-intonation-data predict-intonation-data-lisp-array
;;                   :predict-intonation-data-length output-predict-intonation-data-length-unref))
;;           (list :result-status result-status)))))

;; (cffi:defcfun ("voicevox_decode_data_free" vv-decode-data-free) :void
;;   (decode-data (:pointer :float)))
;; (cffi:defcfun ("voicevox_decode" vv-decode) :int
;;   (length :uintptr)
;;   (phoneme-size :uintptr)
;;   (f0 (:pointer :float))
;;   (phoneme-vector (:pointer :float))
;;   (speaker-id :uint32)
;;   (output-decode-data-length (:pointer :uintptr))
;;   (output-decode-data (:pointer (:pointer :float))))
;; (declaim (ftype (function (&key
;;                            (length uint64)
;;                            (phoneme-size uint64)
;;                            (f0 array)
;;                            (phoneme-vector array)
;;                            (speaker-id uint32)))))
;; (defun decode (length
;;                phoneme-size
;;                f0
;;                phoneme-vector
;;                speaker-id)
;;   (let ((real-phoneme-size (* length phoneme-size)))
;;     (cffi:with-foreign-objects ((c-f0 :float length)
;;                                 (c-phoneme :float real-phoneme-size)
;;                                 (output-decode-data-length :float)
;;                                 (output-decode-data '(:pointer :float)))
;;       (loop for i from 0 below length
;;             do (setf (cffi:mem-aref c-f0 :float i) (aref f0 i)
;;                      (cffi:mem-aref c-phoneme :float i) (aref c-phoneme i)))
;;       (loop for i from (- length 1) below real-phoneme-size
;;             do (setf (cffi:mem-aref c-phoneme :float i) (aref c-phoneme i)))
;;       (let ((result-status
;;               (get-result-from-code
;;                (vv-decode
;;                 length
;;                 phoneme-size
;;                 c-f0
;;                 c-phoneme
;;                 output-decode-data-length
;;                 output-decode-data))))
;;         (if (eq result-status :voicevox-result-ok)
;;             (let ((output-decode-data-length-unref (cffi:mem-ref output-decode-data-length :uintptr))
;;                   (output-decode-data-lisp-array
;;                     (make-array-from-pointer output-decode-data
;;                                              output-decode-data-length-unref
;;                                              '(:pointer :float)
;;                                              :float)))
;;               (list :result-status result-status
;;                     :decode-data-length output-decode-data-length-unref
;;                     :decode-data output-decode-data-lisp-array))
;;             (list :result-status result-status))))))

;; (cffi:defcfun ("voicevox_synthesis" vv-synthesis) :int
;;   (audio-query-json (:pointer :char))
;;   (speaker-id :uint32)
;;   (options (:struct voicevox-synthesis-options))
;;   (output-wav-length (:pointer :uintptr))
;;   (output-wav (:pointer (:pointer :uint8))))
;; (declaim (ftype (function (&key
;;                            (:audio-query-json string)
;;                            (:speaker-id uint32)
;;                            (:enable-interrogative-upspeak boolean))
;;                           list)
;;                 synthesis))
;; (defun synthesis (&key audio-query-json speaker-id enable-interrogative-upspeak)
;;   (cffi:with-foreign-objects ((output-wav-length :uintptr)
;;                               (output-wav '(:pointer :uint8))
;;                               (options-synthesis '(:struct voicevox-synthesis-options)))
;;     (setf
;;      (cffi:foreign-slot-value options-synthesis '(:struct voicevox-synthesis-options) 'enable-interrogative-upspeak)
;;      (if enable-interrogative-upspeak 1 0))
;;     (cffi:with-foreign-strings ((c-audio-query-json audio-query-json))
;;       (let ((result-status
;;               (get-result-from-code
;;                (vv-synthesis
;;                 c-audio-query-json
;;                 speaker-id
;;                 (cffi:mem-ref options-synthesis '(:struct voicevox-synthesis-options))
;;                 output-wav-length
;;                 output-wav))))
;;         (if (eq result-status :voicevox-result-ok)
;;             (let* ((output-wav-length-unref (cffi:mem-ref output-wav-length :uintptr))
;;                    (output-wav-lisp-array (make-array-from-pointer output-wav
;;                                                                    output-wav-length-unref
;;                                                                    '(:pointer :uint8)
;;                                                                    :uint8)))
;;               (list :result-status result-status
;;                     :wav-length output-wav-length-unref
;;                     :wav-bytes output-wav-lisp-array ))
;;             (list :result-status result-status))))))



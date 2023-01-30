(ql:quickload :cffi)
(ql:quickload :cffi-libffi)

(defparameter lib-voicevox-core-path "/home/madosuki/Downloads/voicevox_core/voicevox_core-linux-x64-cpu-0.14.0-preview.4/libvoicevox_core.so")

(cffi:load-foreign-library lib-voicevox-core-path)

(cffi:defcenum voicevox-acceleration-mode
    (:voicevox-acceleration-mode-auto 0)
  (:voicevox-acceleration-mode-cpu 1)
  (:voicevox-acceleration-mode-gpu 2))

(cffi:defcenum voicevox-result-code
    (:voicevox-result-ok 0)
  (:voicevox-result-not-loaded-openjtalk-dict-error 1)
  (:voicevox-result-load-model-error 2)
  (:voicevox-result-get-supported-device-error 3)
  (:voicevox-result-gpu-support-error 4)
  (:voicevox-result-load-metas-error 5)
  (:voicevox-result-uninitialized-status-error 6)
  (:voicevox-result-invalid-speaker-id-error 7)
  (:voicevox-result-invalid-model-index-error 8)
  (:voicevox-result-inference-error 9)
  (:voicevox-result-extract-full-context-label-error 10)
  (:voicevox-result-invalid-utf8-input-error 11)
  (:voicevox-result-parse-kana-error 12)
  (:voicevox-result-invalid-audio-query-error 13))

(cffi:defcstruct voicevox-initialize-options
  (acceleration_mode voicevox-acceleration-mode)
  (cpu_num_threads :int)
  (load_all_models :bool)
  (open_jtalk_dict_dir :string))

(cffi:defcfun ("voicevox_initialize" vv-initialize) :int
  (options :pointer))

(cffi:defcfun ("voicevox_make_default_initialize_options" vv-make-default-initialize-options) (:struct voicevox-initialize-options))

(cffi:defcfun ("voicevox_is_gpu_mode" vv-is-gpu-mode) :bool)
(cffi:defcfun ("voicevox_get_version" vv-get-version) :string)

(defun main ()
  (print (vv-is-gpu-mode))
  (print (vv-get-version))
  (cffi:with-foreign-object (options-s '(:struct voicevox-initialize-options))
    (declare (ignore options-s))
    ;; (setf (cffi:foreign-slot-value options '(:struct voicevox-initialize-options) 'acceleration_mode) :voicevox-acceleration-mode-cpu
    ;;       (cffi:foreign-slot-value options '(:struct voicevox-initialize-options) 'cpu_num_threads) 1
    ;;       (cffi:foreign-slot-value options '(:struct voicevox-initialize-options) 'load_all_models) nil
    ;;       (cffi:foreign-slot-value options '(:struct voicevox-initialize-options) 'open_jtalk_dict_dir) "")

    ;; (print (vv-initialize options))
  ))

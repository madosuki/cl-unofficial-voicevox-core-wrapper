(ql:quickload :cffi)

(defparameter lib-voicevox-core-path "/home/madosuki/Downloads/voicevox_core/voicevox_core-linux-x64-cpu-0.14.0-preview.4/libvoicevox_core.so")

(cffi:load-foreign-library lib-voicevox-core-path)

(cffi:defcenum voicevox-acceleration-mode
  :voicevox-acceleration-mode-auto
  :voicevox-acceleration-mode-cpu
  :voicevox-acceleration-mode-gpu)

(cffi:defcenum voicevox-result-code
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
  :voicevox-result-invalid-audio-query-error)

(cffi:defcstruct voicevox-initialize-options
  (acceleration-mode :int)
  (cpu-num-threads :int)
  (load-all-models :bool))

(cffi:defcfun ("voicevox_initialize" vv-initialize) :int
  (options :pointer))

(defun main ()
  (cffi:with-foreign-object (options '(:struct voicevox-initialize-options))
    (setf (cffi:foreign-slot-value options '(:struct voicevox-initialize-options)
                                   'acceleration-mode)
          2
          (cffi:foreign-slot-value options '(:struct voicevox-initialize-options) 'cpu-num-threads)
          1
          (cffi:foreign-slot-value options '(:struct voicevox-initialize-options) 'load-all-models)
          nil)
    (print (vv-initialize options))))

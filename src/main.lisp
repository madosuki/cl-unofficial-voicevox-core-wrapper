(ql:quickload :cffi)

(defparameter lib-voicevox-core-path "/home/madosuki/Downloads/voicevox_core/voicevox_core-linux-x64-cpu-0.14.0-preview.4/libvoicevox_core.so")

(cffi:load-foreign-library lib-voicevox-core-path)
(cffi:defcfun ("initialize" vv-initialize) :bool
  (use_gpu :bool)
  (cpu_num_threads :int)
  (load_all_modesl :bool))

(defun main ()
  (print (vv-initialize nil 4 nil)))

(defsystem "cl-unoffcial-voicevox-core-wrapper-types"
  :version "0.0.1"
  :author "madosuki"
  :license "MIT"
  :depends-on (:cffi :cffi-libffi)
  :components ((:file "src/types")))

(register-system-packages "cl-unoffcial-voicevox-core-wrapper-types" '(:cl-unoffcial-voicevox-core-wrapper.types))

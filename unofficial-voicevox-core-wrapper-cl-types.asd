(defsystem "unofficial-voicevox-core-wrapper-cl-types"
  :version "0.0.1"
  :author "madosuki"
  :license "MIT"
  :depends-on (:cffi :cffi-libffi)
  :components ((:file "src/types")))

(register-system-packages "unofficial-voicevox-core-wrapper-cl-types" '(:unofficial-voicevox-core-wrapper-cl.types))

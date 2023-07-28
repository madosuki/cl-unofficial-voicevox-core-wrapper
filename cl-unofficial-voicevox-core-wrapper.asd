(defsystem "cl-unofficial-voicevox-core-wrapper"
  :version "0.0.1"
  :author "madosuki"
  :license "MIT"
  :depends-on ("cffi"
               "cffi-libffi"
               "cl-unofficial-voicevox-core-wrapper-types")
  :components ((:module "src"
                :components
                        ((:file "cl-unofficial-voicevox-core-wrapper")))))


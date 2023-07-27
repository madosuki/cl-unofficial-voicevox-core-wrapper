(defsystem "unofficial-voicevox-core-wrapper-cl"
  :version "0.0.1"
  :author "madosuki"
  :license "MIT"
  :depends-on ("cffi"
               "cffi-libffi"
               "jonathan"
               "unofficial-voicevox-core-wrapper-cl-types")
  :components ((:module "src"
                :components
                        ((:file "unofficial-voicevox-core-wrapper-cl")))))


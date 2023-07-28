(defsystem "cl-unoffcial-voicevox-core-wrapper"
  :version "0.0.1"
  :author "madosuki"
  :license "MIT"
  :depends-on ("cffi"
               "cffi-libffi"
               "jonathan"
               "cl-unoffcial-voicevox-core-wrapper-types")
  :components ((:module "src"
                :components
                        ((:file "cl-unoffcial-voicevox-core-wrapper")))))


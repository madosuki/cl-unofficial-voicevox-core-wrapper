
(defpackage :cl-unofficial-voicevox-core-wrapper
  (:use :cl :asdf))
(in-package :cl-unofficial-voicevox-core-wrapper)

(defsystem "cl-unofficial-voicevox-core-wrapper"
  :version "0.0.1"
  :author "madosuki"
  :license "MIT"
  :depends-on ("cffi"
               "cffi-libffi"
               "cl-unofficial-voicevox-core-wrapper-types")
  :components ((:module "src"
                :components
                        ((:file "binding")
                         (:file "wrapper"
                          :depends-on ("binding"))
                         (:file "cl-unofficial-voicevox-core"
                          :depends-on ("wrapper"))))))

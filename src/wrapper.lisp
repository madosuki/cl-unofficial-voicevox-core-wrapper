(in-package :cl-user)
(defpackage :cl-unofficial-voicevox-core-wrapper
  (:use :cl)
  (:import-from
   :cl-unofficial-voicevox-core-wrapper.binding
   
   ;; import from types.lisp
   :uint8
   :uint16
   :uint32
   :voicevox-result-code-type
   :voicevox-acceleration-mode-type
   :voicevox-user-dict-word-type-type
   :voicevox-on-existing-voice-model-id-type

   ;; enums
   :voicevox-acceleration-mode
   :voicevox-on-existing-voice-model-id
   :voicevox-result-code
   :voicevox-user-dict-word-type

   ;; structs
   :open-jtalk-rc
   :voicevox-onnxruntime
   :voicevox-synthesizer
   :voicevox-user-dict
   :voicevox-voice-model-file
   :voicevox-load-onnxruntime-options
   :voicevox-initialize-options
   :voicevox-load-voice-model-options
   :voicevox-synthesis-options
   :voicevox-tts-options
   :voicevox-user-dict-word

   ;; typedefs
   :voicevox-voice-model-id
   :voicevox-style-id

   ;; ONNX Runtime
   :vv-get-onnxruntime-lib-recommended-versioned-filename
   :vv-get-onnxruntime-lib-recommended-unversioned-filename
   :vv-get-onnxruntime-lib-min-required-minor-version
   :vv-get-onnxruntime-lib-max-supported-minor-version
   :vv-make-default-load-onnxruntime-options
   :vv-onnxruntime-get
   :vv-onnxruntime-load-once
   :vv-onnxruntime-init-once

   ;; Open JTalk
   :vv-open-jtalk-rc-new
   :vv-open-jtalk-rc-use-user-dict
   :vv-open-jtalk-rc-analyze
   :vv-open-jtalk-rc-delete

   ;; general
   :vv-make-default-initialize-options
   :vv-get-version
   :vv-audio-query-create-from-accent-phrases
   :vv-audio-query-validate
   :vv-accent-phrase-validate
   :vv-mora-validate
   :vv-score-validate
   :vv-note-validate
   :vv-frame-audio-query-validate
   :vv-frame-phoneme-validate
   :vv-ensure-compatible

   ;; Voice model file
   :vv-voice-model-file-open
   :vv-voice-model-file-id
   :vv-voice-model-file-create-metas-json
   :vv-voice-model-file-delete

   ;; Synthesizer
   :vv-synthesizer-new
   :vv-synthesizer-delete
   :vv-make-default-load-voice-model-options
   :vv-synthesizer-load-voice-model
   :vv-synthesizer-unload-voice-model
   :vv-synthesizer-get-onnxruntime
   :vv-synthesizer-is-gpu-mode
   :vv-synthesizer-is-loaded-voice-model
   :vv-synthesizer-create-metas-json
   :vv-onnxruntime-create-supported-devices-json
   :vv-synthesizer-create-audio-query-from-kana
   :vv-synthesizer-create-audio-query
   :vv-synthesizer-create-accent-phrases-from-kana
   :vv-synthesizer-create-accent-phrases
   :vv-synthesizer-replace-mora-data
   :vv-synthesizer-replace-phoneme-length
   :vv-synthesizer-replace-mora-pitch
   :vv-make-default-synthesis-options
   :vv-synthesizer-synthesis
   :vv-make-default-tts-options
   :vv-synthesizer-tts-from-kana
   :vv-synthesizer-tts
   :vv-synthesizer-create-sing-frame-audio-query
   :vv-synthesizer-create-sing-frame-f0
   :vv-synthesizer-create-sing-frame-volume
   :vv-synthesizer-frame-synthesis

   ;; memory
   :vv-json-free
   :vv-wav-free
   :vv-error-result-to-message

   ;; User dictionary
   :vv-user-dict-word-make
   :vv-user-dict-new
   :vv-user-dict-load
   :vv-user-dict-add-word
   :vv-user-dict-update-word
   :vv-user-dict-remove-word
   :vv-user-dict-to-json
   :vv-user-dict-import
   :vv-user-dict-save
   :vv-user-dict-delete)
  (:export
   ;; utils
   :get-result-from-code
   :error-result-to-message
   :get-version
   :get-status-on-existing-voice-model-id
   
   ;; ONNX Runtime wrapper
   :onnxruntime-class
   :onnxruntime-init
   :onnxruntime-get
   :onnxruntime-ptr
   :onnxruntime-set-filename-to-options

   ;; Open JTalk wrapper
   :open-jtalk-rc-class
   :open-jtalk-rc-init
   :open-jtalk-rc-delete
   :open-jtalk-rc-close
   :open-jtalk-rc-use-user-dict
   :open-jtalk-rc-analyze
   :open-jtalk-rc-ptr

   ;; Synthesizer wrapper
   :voicevox-class
   :voicevox-synthesizer-init
   :voicevox-synthesizer-delete
   :voicevox-synthesizer-load-voice-model
   :voicevox-synthesizer-unload-voice-model
   :voicevox-synthesizer-is-gpu-mode
   :voicevox-synthesizer-is-loaded-voice-model
   :voicevox-synthesizer-create-metas-json
   :voicevox-synthesizer-create-audio-query
   :voicevox-synthesizer-create-audio-query-from-kana
   :voicevox-synthesizer-create-accent-phrases
   :voicevox-synthesizer-create-accent-phrases-from-kana
   :voicevox-synthesizer-replace-mora-data
   :voicevox-synthesizer-replace-phoneme-length
   :voicevox-synthesizer-replace-mora-pitch
   :voicevox-synthesizer-synthesis
   :voicevox-synthesizer-tts
   :voicevox-synthesizer-tts-from-kana
   :voicevox-synthesizer-create-sing-frame-audio-query
   :voicevox-synthesizer-create-sing-frame-f0
   :voicevox-synthesizer-create-sing-frame-volume
   :voicevox-synthesizer-frame-synthesis

   ;; Voice model file wrapper
   :voice-model-file-class
   :voice-model-file-open
   :voice-model-file-id
   :voice-model-file-create-metas-json
   :voice-model-file-delete
   :voice-model-file-close

   ;; User dictionary wrapper
   :user-dict-class
   :user-dict-new
   :user-dict-load
   :user-dict-add-word
   :user-dict-update-word
   :user-dict-remove-word
   :user-dict-to-json
   :user-dict-import
   :user-dict-save
   :user-dict-delete
   :user-dict-ptr))
(in-package :cl-unofficial-voicevox-core-wrapper)

(defvar *loaded-libraries* (make-hash-table :test #'equal))
(defun load-library (path)
  (let ((library (cffi:load-foreign-library path)))
    (setf (gethash path *loaded-libraries*) library)
    library))

(defun close-library (library-or-path)
  (let ((library (if (stringp library-or-path)
                     (gethash library-or-path *loaded-libraries*)
                     library-or-path)))
    (when library
      (cffi:close-foreign-library library)
      (when (stringp library-or-path)
        (remhash library-or-path *loaded-libraries*)))
    library))

(defun get-version ()
  (vv-get-version))

(defun get-status-on-existing-voice-model-id (code)
  (etypecase code
    (keyword code)
    (integer (cffi:foreign-enum-keyword 'voicevox-on-existing-voice-model-id code))))

(defun get-result-from-code (code)
  (etypecase code
    (keyword code)
    (integer (cffi:foreign-enum-keyword 'voicevox-result-code code))))

(defun error-result-to-message (result-code)
  (vv-error-result-to-message result-code))

(defun get-onnxruntime-lib-recommend-versioned-filename ()
  (vv-get-onnxruntime-lib-recommended-versioned-filename))

(defun get-onnxruntime-lib-recommend-unversioned-filename ()
  (vv-get-onnxruntime-lib-recommend-unversioned-filename))

(defun pointer-value (pointer type)
  (cffi:mem-ref pointer type))

(defun json-pointer-to-string-and-free (pointer)
  (unwind-protect
       (cffi:foreign-string-to-lisp pointer)
    (vv-json-free pointer)))

(defun copy-wav-pointer (pointer length)
  (let ((result (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length result)
      (setf (aref result i) (cffi:mem-aref pointer :uint8 i)))))

(defun make-array-from-pointer (target length pointer-type value-type)
  (let ((result (make-array length)))
    (dotimes (i length result)
      (setf (aref result i)
            (cffi:mem-aref (cffi:mem-aref target pointer-type) value-type i)))))

(defclass onnxruntime-class ()
  ((onnxruntime-ptr
    :accessor onnxruntime-ptr
    :initform (cffi:foreign-alloc '(:pointer (:struct voicevox-onnxruntime))))
   (options :accessor options :initform (vv-make-default-load-onnxruntime-options))))

(defmethod onnxruntime-set-filename-to-options ((self onnxruntime-class) filename)
  (setf (cffi:foreign-slot-value (options self) '(:struct voicevox-load-onnxruntime-options) 'filename)
        (cffi:foreign-string-alloc filename)))

(defmethod onnxruntime-init ((self onnxruntime-class))
  (get-result-from-code
   (vv-onnxruntime-load-once (options self)
                             (onnxruntime-ptr self))))

(defun onnxruntime-get ()
  (vv-onnxruntime-get))

(defclass open-jtalk-rc-class ()
  ((open-jtalk-rc-ptr
    :accessor open-jtalk-rc-ptr
    :initform (cffi:foreign-alloc '(:pointer (:struct open-jtalk-rc))))))

(defmethod open-jtalk-rc-init ((self open-jtalk-rc-class) open-jtalk-dic-dir)
  (declare (type string open-jtalk-dic-dir))
  (cffi:with-foreign-string (c-open-jtalk-dic-dir open-jtalk-dic-dir)
    (get-result-from-code
     (vv-open-jtalk-rc-new c-open-jtalk-dic-dir (open-jtalk-rc-ptr self)))))

(defmethod open-jtalk-rc-delete ((self open-jtalk-rc-class))
  (vv-open-jtalk-rc-delete
   (pointer-value (open-jtalk-rc-ptr self) '(:pointer (:struct open-jtalk-rc)))))

(defmethod open-jtalk-rc-close ((self open-jtalk-rc-class))
  (unless (cffi:null-pointer-p (open-jtalk-rc-ptr self))
    (open-jtalk-rc-delete self)
    (cffi:foreign-free (open-jtalk-rc-ptr self))
    (setf (slot-value self 'open-jtalk-rc-ptr) (cffi:null-pointer))))

(defmethod open-jtalk-rc-use-user-dict ((self open-jtalk-rc-class) user-dict)
  (vv-open-jtalk-rc-use-user-dict
   (pointer-value (open-jtalk-rc-ptr self) '(:pointer (:struct open-jtalk-rc)))
   (user-dict-ptr user-dict)))

(defmethod open-jtalk-rc-analyze ((self open-jtalk-rc-class) text)
  (cffi:with-foreign-string (c-text text)
    (cffi:with-foreign-object (output-accent-phrases-json '(:pointer :char))
      (let ((result (get-result-from-code
                     (vv-open-jtalk-rc-analyze
                      (pointer-value (open-jtalk-rc-ptr self) '(:pointer (:struct open-jtalk-rc)))
                      c-text
                      output-accent-phrases-json))))
        (values result
                (when (eq result :voicevox-result-ok)
                  (json-pointer-to-string-and-free
                   (cffi:mem-ref output-accent-phrases-json '(:pointer :char)))))))))

(defclass voicevox-class ()
  ((options :accessor options :initform (vv-make-default-initialize-options))
   (synthesizer
    :accessor synthesizer
    :initform (cffi:foreign-alloc '(:pointer (:struct voicevox-synthesizer))))
   (load-voice-model-options :accessor load-voice-model-options :initform (vv-make-default-load-voice-model-options))))

(defmethod voicevox-synthesizer-init ((self voicevox-class) onnxruntime-instance open-jtalk-instance)
  (get-result-from-code
   (vv-synthesizer-new
    (pointer-value (onnxruntime-ptr onnxruntime-instance) '(:pointer (:struct voicevox-onnxruntime)))
    (pointer-value (open-jtalk-rc-ptr open-jtalk-instance) '(:pointer (:struct open-jtalk-rc)))
    (options self)
    (synthesizer self))))

(defmethod voicevox-synthesizer-delete ((self voicevox-class))
  (vv-synthesizer-delete
   (pointer-value (synthesizer self) '(:pointer (:struct voicevox-synthesizer)))))

(defun synthesizer-pointer (voicevox)
  (pointer-value (synthesizer voicevox) '(:pointer (:struct voicevox-synthesizer))))

(defmethod voicevox-synthesizer-load-voice-model ((self voicevox-class) voice-model-file)
  (get-result-from-code
   (vv-synthesizer-load-voice-model (synthesizer-pointer self)
                                    (voice-model-file-pointer voice-model-file)
                                    (load-voice-model-options self))))

(defmethod voicevox-synthesizer-unload-voice-model ((self voicevox-class) model-id)
  (get-result-from-code
   (vv-synthesizer-unload-voice-model (synthesizer-pointer self) model-id)))

(defmethod voicevox-synthesizer-is-gpu-mode ((self voicevox-class))
  (vv-synthesizer-is-gpu-mode (synthesizer-pointer self)))

(defmethod voicevox-synthesizer-is-loaded-voice-model ((self voicevox-class) model-id)
  (vv-synthesizer-is-loaded-voice-model (synthesizer-pointer self) model-id))

(defmethod voicevox-synthesizer-create-metas-json ((self voicevox-class))
  (json-pointer-to-string-and-free
   (vv-synthesizer-create-metas-json (synthesizer-pointer self))))

(defmacro call-json-output-function (function &rest args)
  (let ((output-json (gensym "OUTPUT-JSON-"))
        (result (gensym "RESULT-"))
        (function-name (if (and (consp function) (eq (first function) 'function))
                           (second function)
                           function)))
    `(cffi:with-foreign-object (,output-json '(:pointer :char))
       (let ((,result (get-result-from-code (,function-name ,@args ,output-json))))
         (values ,result
                 (when (eq ,result :voicevox-result-ok)
                   (json-pointer-to-string-and-free
                    (cffi:mem-ref ,output-json '(:pointer :char)))))))))

(defmethod voicevox-synthesizer-create-audio-query ((self voicevox-class) text style-id)
  (cffi:with-foreign-string (c-text text)
    (call-json-output-function #'vv-synthesizer-create-audio-query
                               (synthesizer-pointer self)
                               c-text
                               style-id)))

(defmethod voicevox-synthesizer-create-audio-query-from-kana ((self voicevox-class) kana style-id)
  (cffi:with-foreign-string (c-kana kana)
    (call-json-output-function #'vv-synthesizer-create-audio-query-from-kana
                               (synthesizer-pointer self)
                               c-kana
                               style-id)))

(defmethod voicevox-synthesizer-create-accent-phrases ((self voicevox-class) text style-id)
  (cffi:with-foreign-string (c-text text)
    (call-json-output-function #'vv-synthesizer-create-accent-phrases
                               (synthesizer-pointer self)
                               c-text
                               style-id)))

(defmethod voicevox-synthesizer-create-accent-phrases-from-kana ((self voicevox-class) kana style-id)
  (cffi:with-foreign-string (c-kana kana)
    (call-json-output-function #'vv-synthesizer-create-accent-phrases-from-kana
                               (synthesizer-pointer self)
                               c-kana
                               style-id)))

(defmethod voicevox-synthesizer-replace-mora-data ((self voicevox-class) accent-phrases-json style-id)
  (cffi:with-foreign-string (c-json accent-phrases-json)
    (call-json-output-function #'vv-synthesizer-replace-mora-data
                               (synthesizer-pointer self)
                               c-json
                               style-id)))

(defmethod voicevox-synthesizer-replace-phoneme-length ((self voicevox-class) accent-phrases-json style-id)
  (cffi:with-foreign-string (c-json accent-phrases-json)
    (call-json-output-function #'vv-synthesizer-replace-phoneme-length
                               (synthesizer-pointer self)
                               c-json
                               style-id)))

(defmethod voicevox-synthesizer-replace-mora-pitch ((self voicevox-class) accent-phrases-json style-id)
  (cffi:with-foreign-string (c-json accent-phrases-json)
    (call-json-output-function #'vv-synthesizer-replace-mora-pitch
                               (synthesizer-pointer self)
                               c-json
                               style-id)))

(defun call-wav-output-function (function &rest args)
  (cffi:with-foreign-object (output-wav-length :uintptr)
    (cffi:with-foreign-object (output-wav '(:pointer :uint8))
      (let ((result (apply function (append args (list output-wav-length output-wav)))))
        (setf result (get-result-from-code result))
        (if (eq result :voicevox-result-ok)
            (let* ((wav-length (cffi:mem-ref output-wav-length :uintptr))
                   (wav-pointer (cffi:mem-ref output-wav '(:pointer :uint8)))
                   (wav-bytes (unwind-protect
                                  (copy-wav-pointer wav-pointer wav-length)
                                (vv-wav-free wav-pointer))))
              (values result wav-bytes wav-length))
            (values result nil 0))))))

(defmethod voicevox-synthesizer-synthesis ((self voicevox-class) audio-query-json style-id
                                           &optional options)
  (cffi:with-foreign-string (c-json audio-query-json)
    (call-wav-output-function #'vv-synthesizer-synthesis
                              (synthesizer-pointer self)
                              c-json
                              style-id
                              (or options (vv-make-default-synthesis-options)))))

(defmethod voicevox-synthesizer-tts ((self voicevox-class) text style-id &optional options)
  (cffi:with-foreign-string (c-text text)
    (call-wav-output-function #'vv-synthesizer-tts
                              (synthesizer-pointer self)
                              c-text
                              style-id
                              (or options (vv-make-default-tts-options)))))

(defmethod voicevox-synthesizer-tts-from-kana ((self voicevox-class) kana style-id &optional options)
  (cffi:with-foreign-string (c-kana kana)
    (call-wav-output-function #'vv-synthesizer-tts-from-kana
                              (synthesizer-pointer self)
                              c-kana
                              style-id
                              (or options (vv-make-default-tts-options)))))

(defmethod voicevox-synthesizer-create-sing-frame-audio-query ((self voicevox-class) score-json style-id)
  (cffi:with-foreign-string (c-score score-json)
    (call-json-output-function #'vv-synthesizer-create-sing-frame-audio-query
                               (synthesizer-pointer self)
                               c-score
                               style-id)))

(defmethod voicevox-synthesizer-create-sing-frame-f0 ((self voicevox-class) score-json frame-audio-query-json style-id)
  (cffi:with-foreign-string (c-score score-json)
    (cffi:with-foreign-string (c-query frame-audio-query-json)
      (call-json-output-function #'vv-synthesizer-create-sing-frame-f0
                                 (synthesizer-pointer self)
                                 c-score
                                 c-query
                                 style-id))))

(defmethod voicevox-synthesizer-create-sing-frame-volume ((self voicevox-class) score-json frame-audio-query-json style-id)
  (cffi:with-foreign-string (c-score score-json)
    (cffi:with-foreign-string (c-query frame-audio-query-json)
      (call-json-output-function #'vv-synthesizer-create-sing-frame-volume
                                 (synthesizer-pointer self)
                                 c-score
                                 c-query
                                 style-id))))

(defmethod voicevox-synthesizer-frame-synthesis ((self voicevox-class) frame-audio-query-json style-id)
  (cffi:with-foreign-string (c-json frame-audio-query-json)
    (call-wav-output-function #'vv-synthesizer-frame-synthesis
                              (synthesizer-pointer self)
                              c-json
                              style-id)))

(defclass voice-model-file-class ()
  ((voice-model-file-ptr
    :accessor voice-model-file-ptr
    :initform (cffi:foreign-alloc '(:pointer (:struct voicevox-voice-model-file))))))

(defmethod voice-model-file-open ((self voice-model-file-class) path)
  (cffi:with-foreign-string (c-path path)
    (get-result-from-code
     (vv-voice-model-file-open c-path (voice-model-file-ptr self)))))

(defun voice-model-file-pointer (voice-model-file)
  (pointer-value (voice-model-file-ptr voice-model-file)
                 '(:pointer (:struct voicevox-voice-model-file))))

(defmethod voice-model-file-id ((self voice-model-file-class))
  (cffi:with-foreign-object (model-id '(:array :uint8 16))
    (vv-voice-model-file-id (voice-model-file-pointer self) model-id)
    (let ((result (make-array 16 :element-type '(unsigned-byte 8))))
      (dotimes (i 16 result)
        (setf (aref result i) (cffi:mem-aref model-id :uint8 i))))))

(defmethod voice-model-file-create-metas-json ((self voice-model-file-class))
  (json-pointer-to-string-and-free
   (vv-voice-model-file-create-metas-json (voice-model-file-pointer self))))

(defmethod voice-model-file-delete ((self voice-model-file-class))
  (vv-voice-model-file-delete (voice-model-file-pointer self)))

(defmethod voice-model-file-close ((self voice-model-file-class))
  (unless (cffi:null-pointer-p (voice-model-file-ptr self))
    (voice-model-file-delete self)
    (cffi:foreign-free (voice-model-file-ptr self))
    (setf (slot-value self 'voice-model-file-ptr) (cffi:null-pointer))))

(defclass user-dict-class ()
  ((user-dict-ptr :accessor user-dict-ptr :initform (cffi:null-pointer))))

(defmethod user-dict-new ((self user-dict-class))
  (setf (user-dict-ptr self) (vv-user-dict-new)))

(defmethod user-dict-load ((self user-dict-class) path)
  (cffi:with-foreign-string (c-path path)
    (get-result-from-code
     (vv-user-dict-load (user-dict-ptr self) c-path))))

(defmethod user-dict-add-word ((self user-dict-class) word)
  (cffi:with-foreign-object (output-word-uuid '(:array :uint8 16))
    (let ((result (get-result-from-code
                   (vv-user-dict-add-word (user-dict-ptr self) word output-word-uuid)))
          (uuid (make-array 16 :element-type '(unsigned-byte 8))))
      (dotimes (i 16)
        (setf (aref uuid i) (cffi:mem-aref output-word-uuid :uint8 i)))
      (values result uuid))))

(defmethod user-dict-update-word ((self user-dict-class) word-uuid word)
  (get-result-from-code
   (vv-user-dict-update-word (user-dict-ptr self) word-uuid word)))

(defmethod user-dict-remove-word ((self user-dict-class) word-uuid)
  (get-result-from-code
   (vv-user-dict-remove-word (user-dict-ptr self) word-uuid)))

(defmethod user-dict-to-json ((self user-dict-class))
  (call-json-output-function #'vv-user-dict-to-json (user-dict-ptr self)))

(defmethod user-dict-import ((self user-dict-class) other-dict)
  (get-result-from-code
   (vv-user-dict-import (user-dict-ptr self) (user-dict-ptr other-dict))))

(defmethod user-dict-save ((self user-dict-class) path)
  (cffi:with-foreign-string (c-path path)
    (get-result-from-code
     (vv-user-dict-save (user-dict-ptr self) c-path))))

(defmethod user-dict-delete ((self user-dict-class))
  (vv-user-dict-delete (user-dict-ptr self))
  (setf (user-dict-ptr self) (cffi:null-pointer)))

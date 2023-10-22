(in-package #:org.shirakumo.fraf.turbojpeg)

(define-condition jpeg-condition (condition)
  ((jpeg :initarg :jpeg :reader jpeg)
   (message :initarg :message :reader message))
  (:report (lambda (c s) (format s "~a failed to perform:~%~a"
                                 (jpeg c) (message c)))))

(define-condition jpeg-error (jpeg-condition error) ())
(define-condition jpeg-warning (jpeg-condition warning) ())

(defun init ()
  (unless (cffi:foreign-library-loaded-p 'turbo:libturbojpeg)
    (cffi:load-foreign-library 'turbo:libturbojpeg)))

(defmethod free (ptr)
  (check-type ptr cffi:foreign-pointer)
  (turbo:free ptr))

(defun %write-file (jpeg buf size path)
  (let ((file (cffi:foreign-funcall "fopen" :string (uiop:native-namestring path) :string "wb" :pointer)))
    (unwind-protect
         (progn
           (when (cffi:null-pointer-p file)
             (error 'jpeg-error :jpeg jpeg :message "Failed to open file to write to."))
           (when (< (cffi:foreign-funcall "fwrite" :pointer buf :size size :size 1 :pointer file :int) 1)
             (error 'jpeg-error :jpeg jpeg :message "Failed to write to file."))
           destination)
      (cffi:foreign-funcall "fclose" :pointer file :int))))

(defclass jpeg ()
  ((handle :initarg :handle :initform NIL :accessor handle)))

(defmethod report-error ((jpeg jpeg))
  (case (turbo:error-type (handle jpeg))
    (:warning (warn 'jpeg-warning :jpeg jpeg :message (turbo:error-string (handle jpeg))))
    (:fatal (error 'jpeg-error :jpeg jpeg :message (turbo:error-string (handle jpeg))))))

(defun test-error (jpeg result)
  (if (< result 0)
      (report-error jpeg)
      result))

(defmacro check-error (form)
  `(test-error ,(second form)
               (,(first form) (handle ,(second form)) ,@(cddr form))))

(defmacro %set-boolean (name)
  `(when ,(intern (format NIL "~a-~a" name 'p))
     (check-error (turbo:set-parameter jpeg ,(intern (string name) "KEYWORD") ,name))))

(defmacro %set-property (name &optional enum)
  `(when ,name
     (check-error (turbo:set-parameter jpeg ,(intern (string name) "KEYWORD")
                                       ,(if enum
                                            `(cffi:foreign-enum-value ',enum ,name)
                                            name)))))

(defmethod shared-initialize :after ((jpeg jpeg) slots &key (stop-on-warning NIL stop-on-warning-p)
                                                            (bottom-up NIL bottom-up-p)
                                                            (progressive NIL progressive-p)
                                                            (arithmetic NIL arithmetic-p))
  (init)
  (unless (handle jpeg)
    (let ((handle (make-handle jpeg)))
      (if (cffi:null-pointer-p handle)
          (error "Failed to allocate JPEG handle.")
          (setf (handle jpeg) handle))))
  (%set-boolean stop-on-warning)
  (%set-boolean arithmetic)
  (%set-boolean bottom-up)
  (%set-boolean progressive))

(defmethod free ((jpeg jpeg))
  (when (handle jpeg)
    (turbo:destroy (handle jpeg))
    (setf (handle jpeg) NIL)))

(defmacro define-property-wrapper (class name &optional (value 'value) &body transform)
  (destructuring-bind (method &optional (property (intern (string method) "KEYWORD")))
      (if (listp name) name (list name))
    `(defmethod ,method ((,class ,class))
       (let ((,value (turbo:get-parameter (handle ,class) ,property)))
         ,(or transform value)))))

(defclass compressor (jpeg)
  ())

(defmethod make-handle ((_ compressor))
  (turbo:make-handle :compress))

(defmethod shared-initialize :after ((jpeg compressor) slots &key (no-realloc NIL no-realloc-p)
                                                                  (fast-dct NIL fast-dct-p)
                                                                  (optimize NIL optimize-p)
                                                                  (lossless NIL lossless-p)
                                                                  quality
                                                                  subsampling
                                                                  color-space
                                                                  restart-blocks
                                                                  restart-rows
                                                                  x-density
                                                                  y-density)
  (%set-boolean no-realloc)
  (%set-property quality)
  (%set-property subsampling turbo:chrominance-sampling)
  (%set-property color-space turbo:color-space)
  (%set-boolean fast-dct)
  (%set-boolean optimize)
  (%set-boolean lossless)
  (%set-property restart-blocks)
  (%set-property restart-rows)
  (%set-property x-density)
  (%set-property y-density))

(defmethod save-image (dst buffer width height (jpeg compressor) &key (pixel-format :rgb)
                                                                      pitch
                                                                      (bit-depth 8)
                                                                      size)
  (unless pitch
    (setf pitch (* (turbo:pixel-size pixel-format) (width jpeg))))
  (cffi:with-foreign-objects ((dst-ptr :pointer)
                              (size-ptr :size))
    (setf (cffi:mem-ref dst-ptr :pointer) (if dst dst (cffi:null-pointer)))
    (setf (cffi:mem-ref size-ptr :size) (if dst size 0))
    (let ((result (ecase bit-depth
                    (8 (turbo:compress (handle jpeg) buffer width pitch height pixel-format dst-ptr size-ptr))
                    (12 (turbo:compress/12 (handle jpeg) buffer width pitch height pixel-format dst-ptr size-ptr))
                    (16 (turbo:compress/16 (handle jpeg) buffer width pitch height pixel-format dst-ptr size-ptr)))))
      (test-error jpeg result)
      (values (cffi:mem-aref dst-ptr :pointer)
              (cffi:mem-aref size-ptr :size)))))

(defmethod save-image ((destination vector) src width height (jpeg compressor) &rest args &key size &allow-other-keys)
  (cffi:with-pointer-to-vector-data (ptr destination)
    (apply #'save-image ptr src width height jpeg :size (or size (length destination)) args)))

(defmethod save-image ((destination pathname) src width height (jpeg compressor) &rest args &key &allow-other-keys)
  (multiple-value-bind (buf size) (apply #'save-image NIL src width height jpeg args)
    (unwind-protect (%write-file jpeg buf size destination)
      (turbo:free buf))))

(defmethod save-image (destination (source vector) width height (jpeg compressor) &rest args &key &allow-other-keys)
  (cffi:with-pointer-to-vector-data (ptr source)
    (apply #'save-image ptr source width height jpeg args)))

(defmethod save-image (destination src width height (jpeg (eql T)) &rest args &key &allow-other-keys)
  (let ((jpeg (make-instance 'compressor)))
    (unwind-protect (apply #'save-image destination src width height jpeg args)
      (free jpeg))))

(defclass decompressor (jpeg)
  ())

(defmethod make-handle ((_ decompressor))
  (turbo:make-handle :decompress))

(defmethod shared-initialize :after ((jpeg decompressor) slots &key (fast-dct NIL fast-dct-p)
                                                                    (fast-upsample NIL fast-upsample-p)
                                                                    (optimize NIL optimize-p)
                                                                    (lossless NIL lossless-p)
                                                                    subsampling
                                                                    color-space
                                                                    scan-limit
                                                                    x-density
                                                                    y-density)
  (%set-boolean fast-upsample)
  (%set-property subsampling turbo:chrominance-sampling)
  (%set-property color-space turbo:color-space)
  (%set-boolean fast-dct)
  (%set-boolean optimize)
  (%set-boolean lossless)
  (%set-property scan-limit)
  (%set-property x-density)
  (%set-property y-density))

(define-property-wrapper decompressor width)
(define-property-wrapper decompressor height)
(define-property-wrapper decompressor precision)

(defmethod load-image (ptr (jpeg decompressor) &key (pixel-format :rgb)
                                                    pitch
                                                    (bit-depth 8)
                                                    size
                                                    buffer)
  (check-type ptr cffi:foreign-pointer)
  (check-error (turbo:decompress-header jpeg ptr size))
  (unless pitch
    (setf pitch (* (turbo:pixel-size pixel-format) (width jpeg))))
  (let ((buffer-size (* pitch (height jpeg))))
    (labels ((load-image/ptr (buffer)
               (let ((result (ecase bit-depth
                               (8 (turbo:decompress (handle jpeg) ptr size buffer pitch pixel-format))
                               (12 (turbo:decompress/12 (handle jpeg) ptr size buffer pitch pixel-format))
                               (16 (turbo:decompress/16 (handle jpeg) ptr size buffer pitch pixel-format)))))
                 (test-error jpeg result)
                 (values buffer
                         (width jpeg)
                         (height jpeg)
                         pixel-format
                         buffer-size)))
             (load-image/vec (vec)
               (cffi:with-pointer-to-vector-data (ptr vec)
                 (multiple-value-bind (ptr w h p s) (load-image/ptr ptr)
                   (declare (ignore ptr))
                   (values vec w h p s)))))
      (etypecase buffer
        (null
         (load-image/ptr (turbo:alloc (ceiling (* bit-depth buffer-size) 8))))
        (cffi:foreign-pointer
         (load-image/ptr buffer))
        (vector
         (load-image/vec (if (<= buffer-size (length buffer))
                             buffer
                             (adjust-array buffer buffer-size))))
        ((eql :vector)
         (load-image/vec (ecase bit-depth
                           (8 (make-array buffer-size :element-type '(unsigned-byte 8)))
                           (16 (make-array buffer-size :element-type '(unsigned-byte 16))))))))))

(defmethod load-image ((source pathname) (jpeg decompressor) &rest args &key &allow-other-keys)
  (let (vec)
    (with-open-file (stream source :element-type '(unsigned-byte 8))
      (setf vec (make-array (file-length stream) :element-type '(unsigned-byte 8)))
      (read-sequence vec stream))
    (apply #'load-image vec jpeg args)))

(defmethod load-image ((source vector) (jpeg decompressor) &rest args &key size &allow-other-keys)
  (cffi:with-pointer-to-vector-data (ptr source)
    (apply #'load-image ptr jpeg :size (or size (length source)) args)))

(defmethod load-image (source (jpeg (eql T)) &rest args &key &allow-other-keys)
  (let ((jpeg (make-instance 'decompressor)))
    (unwind-protect (apply #'load-image source jpeg args)
      (free jpeg))))

(defclass transformer (jpeg)
  ())

(defmethod make-handle ((_ transformer))
  (turbo:make-handle :transform))

(defmethod shared-initialize :after ((jpeg transformer) slots &key (no-realloc NIL no-realloc-p)
                                                                   subsampling
                                                                   scan-limit)
  (%set-boolean no-realloc)
  (%set-property subsampling turbo:chrominance-sampling)
  (%set-property scan-limit))

(defmethod transform-image (source destination operation (jpeg transformer) &key source-size
                                                                                 destination-size
                                                                                 perfect
                                                                                 trim
                                                                                 crop
                                                                                 gray
                                                                                 progressive
                                                                                 copy-none
                                                                                 arithmetic
                                                                                 optimize)
  (check-type source cffi:foreign-pointer)
  (cffi:with-foreign-objects ((buf-ptr :pointer)
                              (size-ptr :size)
                              (transform '(:struct turbo:transform)))
    (setf (cffi:mem-ref buf-ptr :pointer) (if destination destination (cffi:null-pointer)))
    (setf (cffi:mem-ref size-ptr :size) (if destination destination-size 0))
    (setf (turbo:transform-operation transform) operation)
    (let ((options ()))
      (when perfect (push :perfect options))
      (when trim (push :trim options))
      (when gray (push :gray options))
      (when progressive (push :progressive options))
      (when copy-none (push :copy-none options))
      (when arithmetic (push :arithmetic options))
      (when optimize (push :optimize options))
      (when crop
        (push :crop options)
        (destructuring-bind (x y w h) crop
          (setf (turbo:transform-region transform) (list :x x :y y :w w :h h))))
      (setf (turbo:transform-options transform) options))
    (check-error (turbo:transform jpeg source source-size 1 buf-ptr size-ptr transform))
    (values (cffi:mem-ref buf-ptr :pointer)
            (cffi:mem-ref size-ptr :size))))

(defmethod transform-image ((source vector) destination operation (jpeg transformer) &rest args &key source-size &allow-other-keys)
  (cffi:with-pointer-to-vector-data (ptr source)
    (apply #'transform-image ptr destination operation jpeg :source-size (or source-size (length source)) args)))

(defmethod transform-image (source (destination vector) operation (jpeg transformer) &rest args &key destination-size &allow-other-keys)
  (cffi:with-pointer-to-vector-data (ptr source)
    (apply #'transform-image source ptr operation jpeg :destination-size (or destination-size (length source)) args)))

(defmethod transform-image (source (destination pathname) operation (jpeg transformer) &rest args &key &allow-other-keys)
  (multiple-value-bind (buf size) (apply #'transform-image source NIL operation jpeg args)
    (unwind-protect (%write-file jpeg buf size destination)
      (turbo:free buf))))

(defmethod transform-image ((source pathname) destination operation (jpeg transformer) &rest args &key &allow-other-keys)
  (let (vec)
    (with-open-file (stream source :element-type '(unsigned-byte 8))
      (setf vec (make-array (file-length stream) :element-type '(unsigned-byte 8)))
      (read-sequence vec stream))
    (apply #'transform-image vec destination operation jpeg args)))

(defmethod transform-image (source destination operation (jpeg (eql T)) &rest args &key &allow-other-keys)
  (let ((jpeg (make-instance 'transformer)))
    (unwind-protect (apply #'transform-image source destination operation jpeg args)
      (free jpeg))))

(in-package #:org.shirakumo.fraf.turbojpeg)

(define-condition jpeg-condition (condition)
  ((jpeg :initarg :jpeg :reader jpeg)
   (message :initarg :message :reader message))
  (:report (lambda (c s) (format s "~a failed to perform:~%~a"
                                 (jpeg c) (message c)))))

(define-condition jpeg-error (jpeg-condition error) ())
(define-condition jpeg-warning (jpeg-condition warning) ())

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

(defun init ()
  (unless (cffi:foreign-library-loaded-p 'turbo:libturbojpeg)
    (cffi:load-foreign-library 'turbo:libturbojpeg)))

(defclass jpeg ()
  ((handle :initarg :handle :initform NIL :accessor handle)))

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
  (when (handle compressor)
    (turbo:destroy (handle compressor))
    (setf (handle compressor) NIL)))

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

(defmethod save-image ((path pathname) buffer width height (jpeg compressor) &key (pixel-format :rgb)
                                                                                  (pitch 0)
                                                                                  (bit-depth 8))
  (let ((result (ecase bit-depth
                  (8 (turbo:save-image (handle jpeg) (uiop:native-namestring path) buffer width pitch height pixel-format))
                  (12 (turbo:save-image/12 (handle jpeg) (uiop:native-namestring path) buffer width pitch height pixel-format))
                  (16 (turbo:save-image/16 (handle jpeg) (uiop:native-namestring path) buffer width pitch height pixel-format)))))
    (test-error jpeg result)
    path))

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
    (apply #'save-image ptr src width height jpeg :size (or size (length source)) args)))

(defmethod save-image (destination (source vector) width height (jpeg compressor) &rest args &key size &allow-other-keys)
  (cffi:with-pointer-to-vector-data (ptr source)
    (apply #'save-image destination source width height jpeg args)))

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

(defmethod load-image ((path pathname) (jpeg decompressor) &key pixel-format
                                                                (alignment 1)
                                                                (bit-depth 8))
  (cffi:with-foreign-objects ((width :int)
                              (height :int)
                              (format :int))
    (setf (cffi:mem-ref format 'turbo:pixel-format) (or pixel-format :unknown))
    (let ((value (ecase bit-depth
                   (8 (turbo:load-image (handle jpeg) (uiop:native-namestring path) width alignment height format))
                   (12 (turbo:load-image/12 (handle jpeg) (uiop:native-namestring path) width alignment height format))
                   (16 (turbo:load-image/16 (handle jpeg) (uiop:native-namestring path) width alignment height format)))))
      (when (cffi:null-pointer-p value)
        (report-error jpeg))
      (values value
              (cffi:mem-ref width :int)
              (cffi:mem-ref height :int)
              (cffi:mem-ref pixel-format 'turbo:pixel-format)
              (* (turbo:pixel-size (cffi:mem-ref format 'turbo:pixel-format))
                 (cffi:mem-ref width :int)
                 (cffi:mem-ref height :int))))))

(defmethod load-image (ptr (jpeg decompressor) &key (pixel-format :rgb)
                                                    pitch
                                                    (bit-depth 8)
                                                    size
                                                    buffer)
  (check-type ptr cffi:foreign-pointer)
  (check-error (turbo:decompress-header jpeg ptr size))
  (unless pitch
    (setf pitch (* (turbo:pixel-size pixel-format) (width jpeg))))
  (unless buffer
    (setf buffer (cffi:foreign-alloc :uint8 :count (* pitch (height jpeg)))))
  (let ((result (ecase bit-depth
                  (8 (turbo:decompress (handle jpeg) ptr size buffer pitch pixel-format))
                  (12 (turbo:decompress/12 (handle jpeg) ptr size buffer pitch pixel-format))
                  (16 (turbo:decompress/16 (handle jpeg) ptr size buffer pitch pixel-format)))))
    (test-error jpeg result)
    (values buffer
            (width jpeg)
            (height jpeg)
            pixel-format
            (* pitch (height jpeg)))))

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

(defmethod transform-image (source destination operation (jpeg (eql T)) &rest args &key &allow-other-keys)
  (let ((jpeg (make-instance 'transformer)))
    (unwind-protect (apply #'transform-image source destination operation jpeg args)
      (free jpeg))))

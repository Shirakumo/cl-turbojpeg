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

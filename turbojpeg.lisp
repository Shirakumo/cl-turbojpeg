(in-package #:org.shirakumo.fraf.turbojpeg.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

(cffi:define-foreign-library libturbojpeg
  (:darwin (:or #+X86 "libturbojpeg-mac-i686.dylib"
                #+X86-64 "libturbojpeg-mac-amd64.dylib"
                #+ARM64 "libturbojpeg-mac-arm64.dylib"))
  (:unix (:or #+X86 "libturbojpeg-lin-i686.so"
              #+X86-64 "libturbojpeg-lin-amd64.so"))
  (:windows (:or #+X86 "libturbojpeg-win-i686.dll"
                 #+X86-64 "libturbojpeg-win-amd64.dll"))
  (T (:or (:default "libturbojpeg") (:default "turbojpeg") (:default "libjpeg") (:default "jpeg"))))


(cffi:defcenum init-type
  :compress
  :decompress
  :transform)

(cffi:defcenum chrominance-sampling
  :444
  :422
  :420
  :gray
  :440
  :411
  :441
  (:unknown -1))

(defun mcu-width (sample)
  (ecase sample
    ((0 :444) 8)
    ((1 :422) 16)
    ((2 :420) 16)
    ((3 :gray) 8)
    ((4 :440) 8)
    ((5 :411) 32)
    ((6 :441) 8)))

(defun mcu-height (sample)
  (ecase sample
    ((0 :444) 8)
    ((1 :422) 8)
    ((2 :420) 16)
    ((3 :gray) 8)
    ((4 :440) 16)
    ((5 :411) 8)
    ((6 :441) 32)))

(cffi:defcenum pixel-format
  :rgb
  :bgr
  :rgbx
  :bgrx
  :xbgr
  :xrgb
  :gray
  :rgba
  :bgra
  :abgr
  :argb
  :cmyk
  (:unknown -1))

(defun red-offset (format)
  (ecase format
    ((0 :rgb) 0)
    ((1 :bgr) 2)
    ((2 :rgbx) 0)
    ((3 :bgrx) 2)
    ((4 :xbgr) 3)
    ((5 :xrgb) 1)
    ((6 :gray) NIL)
    ((7 :rgba) 0)
    ((8 :bgra) 2)
    ((9 :abgr) 3)
    ((10 :argb) 1)
    ((11 :cmyk) NIL)))

(defun green-offset (format)
  (ecase format
    ((0 :rgb) 1)
    ((1 :bgr) 1)
    ((2 :rgbx) 1)
    ((3 :bgrx) 1)
    ((4 :xbgr) 2)
    ((5 :xrgb) 2)
    ((6 :gray) NIL)
    ((7 :rgba) 1)
    ((8 :bgra) 1)
    ((9 :abgr) 2)
    ((10 :argb) 2)
    ((11 :cmyk) NIL)))

(defun blue-offset (format)
  (ecase format
    ((0 :rgb) 2)
    ((1 :bgr) 0)
    ((2 :rgbx) 2)
    ((3 :bgrx) 0)
    ((4 :xbgr) 1)
    ((5 :xrgb) 3)
    ((6 :gray) NIL)
    ((7 :rgba) 2)
    ((8 :bgra) 0)
    ((9 :abgr) 1)
    ((10 :argb) 3)
    ((11 :cmyk) NIL)))

(defun alpha-offset (format)
  (ecase format
    ((0 :rgb) NIL)
    ((1 :bgr) NIL)
    ((2 :rgbx) NIL)
    ((3 :bgrx) NIL)
    ((4 :xbgr) NIL)
    ((5 :xrgb) NIL)
    ((6 :gray) NIL)
    ((7 :rgba) 3)
    ((8 :bgra) 3)
    ((9 :abgr) 0)
    ((10 :argb) 0)
    ((11 :cmyk) NIL)))

(defun pixel-size (format)
  (ecase format
    ((0 :rgb) 3)
    ((1 :bgr) 3)
    ((2 :rgbx) 4)
    ((3 :bgrx) 4)
    ((4 :xbgr) 4)
    ((5 :xrgb) 4)
    ((6 :gray) 1)
    ((7 :rgba) 4)
    ((8 :bgra) 4)
    ((9 :abgr) 4)
    ((10 :argb) 4)
    ((11 :cmyk) 4)))

(cffi:defcenum color-space
  :rgb
  :ycbcr
  :gray
  :cmyk
  :ycck)

(cffi:defcenum parameter
  :stop-on-warning
  :bottom-up
  :no-realloc
  :quality
  :subsampling
  :width
  :height
  :precision
  :color-space
  :fast-upsample
  :fast-dct
  :optimize
  :progressive
  :scan-limit
  :arithmetic
  :lossless
  :lossless-psv
  :lossless-pt
  :restart-blocks
  :restart-rows
  :x-density
  :y-density
  :density-units)

(cffi:defcenum error-type
  :warning
  :fatal)

(cffi:defcenum operation
  :none
  :horizontal-flip
  :vertical-flip
  :transpose
  :transverse
  :rotate-90
  :rotate-180
  :rotate-270)

(cffi:defbitfield option
  (:perfect     #b000000001)
  (:trim        #b000000010)
  (:crop        #b000000100)
  (:gray        #b000001000)
  (:no-output   #b000010000)
  (:progressive #b000100000)
  (:copy-none   #b001000000)
  (:arithmetic  #b010000000)
  (:optimize    #b100000000))

(cffi:defcstruct (scaling-factor :conc-name scaling-factor-)
  (numerator :int)
  (denominator :int))

(cffi:defcstruct (region :conc-name region-)
  (x :int)
  (y :int)
  (w :int)
  (h :int))

(cffi:defcstruct (transform :conc-name transform-)
  (region (:struct region))
  (operatino operation)
  (options option)
  (data :pointer)
  (custom-filter :pointer))

(cffi:defcfun ("tj3Init" make-handle) :pointer
  (type init-type))

(cffi:defcfun ("tj3Set" set-parameter) :int
  (handle :pointer)
  (parameter parameter)
  (value :int))

(cffi:defcfun ("tj3Get" get-parameter) :int
  (handle :pointer)
  (parameter parameter))

(cffi:defcfun ("tj3Compress8" compress) :int
  (handle :pointer)
  (source-buffer :pointer)
  (width :int)
  (pitch :int)
  (height :int)
  (pixel-format pixel-format)
  (jpeg-buffer :pointer)
  (size :pointer))

(cffi:defcfun ("tj3Compress12" compress/12) :int
  (handle :pointer)
  (source-buffer :pointer)
  (width :int)
  (pitch :int)
  (height :int)
  (pixel-format pixel-format)
  (jpeg-buffer :pointer)
  (size :pointer))

(cffi:defcfun ("tj3Compress16" compress/16) :int
  (handle :pointer)
  (source-buffer :pointer)
  (width :int)
  (pitch :int)
  (height :int)
  (pixel-format pixel-format)
  (jpeg-buffer :pointer)
  (size :pointer))

(cffi:defcfun ("tj3CompressFromYUV8" compress-from-yuv) :int
  (handle :pointer)
  (source-buffer :pointer)
  (width :int)
  (align :int)
  (height :int)
  (jpeg-buffer :pointer)
  (size :pointer))

(cffi:defcfun ("tj3CompressFromYUVPlanes8" compress-from-yuv-planes) :int
  (handle :pointer)
  (source-buffer :pointer)
  (width :int)
  (strides :pointer)
  (height :int)
  (jpeg-buffer :pointer)
  (size :pointer))

(cffi:defcfun ("tj3JPEGBufSize" jpeg-buffer-size) :size
  (width :int)
  (height :int)
  (subsamp chrominance-sampling))

(cffi:defcfun ("tj3YUVBufSize" yuv-buffer-size) :size
  (width :int)
  (align :int)
  (height :int)
  (subsamp chrominance-sampling))

(cffi:defcfun ("tj3YUVPlaneSize" yuv-plane-size) :size
  (width :int)
  (stride :int)
  (height :int)
  (subsamp chrominance-sampling))

(cffi:defcfun ("tj3YUVPlaneWidth" yuv-plane-width) :size
  (component-id :int)
  (width :int)
  (subsamp chrominance-sampling))

(cffi:defcfun ("tj3EncodeYUV8" encode-yuv) :int
  (handle :pointer)
  (src-buffer :pointer)
  (width :int)
  (pitch :int)
  (height :int)
  (pixel-format pixel-format)
  (dst-buffer :pointer)
  (align :int))

(cffi:defcfun ("tj3EncodeYUVPlanes8" encode-yuv-planes) :int
  (handle :pointer)
  (src-buffer :pointer)
  (width :int)
  (pitch :int)
  (height :int)
  (pixel-format pixel-format)
  (dst-buffer :pointer)
  (strides :pointer))

(cffi:defcfun ("tj3DecompressHeader" decompress-header) :int
  (handle :pointer)
  (buffer :pointer)
  (size :size))

(cffi:defcfun ("tj3GetScalingFactors" get-scaling-factors) :pointer
  (count :pointer))

;; (cffi:defcfun ("tj3SetScalingFactor" set-scaling-factor) :int
;;   (handle :pointer)
;;   (factor (:struct scaling-factor)))

;; (cffi:defcfun ("tj3SetCroppingRegion" set-cropping-region) :int
;;   (handle :pointer)
;;   (region (:struct cropping-region)))

(cffi:defcfun ("tj3Decompress8" decompress) :int
  (handle :pointer)
  (jpeg-buffer :pointer)
  (jpeg-size :size)
  (dst-buffer :pointer)
  (pitch :int)
  (pixel-format pixel-format))

(cffi:defcfun ("tj3Decompress12" decompress/12) :int
  (handle :pointer)
  (jpeg-buffer :pointer)
  (jpeg-size :size)
  (dst-buffer :pointer)
  (pitch :int)
  (pixel-format pixel-format))

(cffi:defcfun ("tj3Decompress16" decompress/16) :int
  (handle :pointer)
  (jpeg-buffer :pointer)
  (jpeg-size :size)
  (dst-buffer :pointer)
  (pitch :int)
  (pixel-format pixel-format))

(cffi:defcfun ("tj3DecompressToYUV8" decompress-to-yuv) :int
  (handle :pointer)
  (jpeg-buffer :pointer)
  (jpeg-size :size)
  (dst-buffer :pointer)
  (align :int))

(cffi:defcfun ("tj3DecompressToYUVPlanes8" decompress-to-yuv-planes) :int
  (handle :pointer)
  (jpeg-buffer :pointer)
  (jpeg-size :size)
  (dst-buffer :pointer)
  (strides :pointer))

(cffi:defcfun ("tj3DecodeYUV8" decode-yuv) :int
  (handle :pointer)
  (src-buffer :pointer)
  (align :int)
  (dst-buffer :pointer)
  (width :int)
  (pitch :int)
  (height :int)
  (pixel-format pixel-format))

(cffi:defcfun ("tj3DecodeYUVPlanes8" decode-yuv-planes) :int
  (handle :pointer)
  (src-buffer :pointer)
  (strides :pointer)
  (dst-buffer :pointer)
  (width :int)
  (pitch :int)
  (height :int)
  (pixel-format pixel-format))

(cffi:defcfun ("tj3Transform" transform) :int
  (handle :pointer)
  (jpeg-buffer :pointer)
  (jpeg-size :size)
  (dst-buffers :pointer)
  (dst-sizes :pointer)
  (transforms :pointer))

(cffi:defcfun ("tj3Destroy" destroy) :void
  (handle :pointer))

(cffi:defcfun ("tj3Alloc" alloc) :pointer
  (bytes :size))

(cffi:defcfun ("tj3LoadImage8" load-image) :pointer
  (handle :pointer)
  (filename :string)
  (width :pointer)
  (align :int)
  (height :pointer)
  (pixel-format :pointer))

(cffi:defcfun ("tj3LoadImage12" load-image/12) :pointer
  (handle :pointer)
  (filename :string)
  (width :pointer)
  (align :int)
  (height :pointer)
  (pixel-format :pointer))

(cffi:defcfun ("tj3LoadImage16" load-image/16) :pointer
  (handle :pointer)
  (filename :string)
  (width :pointer)
  (align :int)
  (height :pointer)
  (pixel-format :pointer))

(cffi:defcfun ("tj3SaveImage8" save-image) :pointer
  (handle :pointer)
  (filename :string)
  (width :pointer)
  (pitch :int)
  (height :pointer)
  (pixel-format :pointer))

(cffi:defcfun ("tj3SaveImage12" save-image/12) :pointer
  (handle :pointer)
  (filename :string)
  (width :pointer)
  (pitch :int)
  (height :pointer)
  (pixel-format :pointer))

(cffi:defcfun ("tj3SaveImage16" save-image/16) :pointer
  (handle :pointer)
  (filename :string)
  (width :pointer)
  (pitch :int)
  (height :pointer)
  (pixel-format :pointer))

(cffi:defcfun ("tj3Free" free) :void
  (buffer :pointer))

(cffi:defcfun ("tj3GetErrorStr" error-string) :string
  (handle :pointer))

(cffi:defcfun ("tj3GetErrorCode" error-code) :int
  (handle :pointer))

(in-package #:org.shirakumo.fraf.jpeg.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

(cffi:define-foreign-library libjpeg
    (:android
     (:or "libjpeg.so"
          #+X86 "libjpeg-android-i686.so"
          #+X86-64 "libjpeg-android-amd64.so"
          #+ARM64 "libjpeg-android-arm64.so"
          #+(and ARM (not ARM64)) "libjpeg-android-arm7a.so"))
  (:darwin (:or #+X86 "libjpeg-mac-i686.dylib"
                #+X86-64 "libjpeg-mac-amd64.dylib"
                #+ARM64 "libjpeg-mac-arm64.dylib"))
  (:unix (:or #+X86 "libjpeg-lin-i686.so"
              #+X86-64 "libjpeg-lin-amd64.so"))
  (:windows (:or #+X86 "libjpeg-win-i686.dll"
                 #+X86-64 "libjpeg-win-amd64.dll"))
  (T (:or (:default "libjpeg") (:default "jpeg"))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant DCT-SIZE 8)
  (defconstant DCT-SIZE-2 64)
  (defconstant QUANTIZATION-TABLES 4)
  (defconstant HUFFMAN-TABLES 4)
  (defconstant ARITH-CODING-TABLES 18)
  (defconstant MAX-SCAN-COMPONENTS 4)
  (defconstant MAX-SAMPLING-FACTOR 4)
  (defconstant MAX-BLOCKS-IN-MCU 10))

(cffi:defcenum color-space
  :unknown
  :grayscale
  :rgb
  :ybcbr
  :cmyk
  :ycck
  :ext-rgb
  :ext-rgbx
  :ext-bgr
  :ext-bgrx
  :ext-xbgr
  :ext-xrgb
  :ext-rgba
  :ext-bgra
  :ext-abgr
  :ext-argb
  :rgb565)

(cffi:defcenum dct-method
  :slow
  :fast
  :float)

(cffi:defcenum dither-mode
  :none
  :ordered
  :floyd-steinberg)

(cffi:defcenum read-header-return
  (:suspended 0)
  (:header-ok 1)
  (:header-tables-only 2))

(cffi:defcenum consume-input-return
  (:suspended 0)
  (:reached-start-of-scan 1)
  (:reached-end-of-image 2)
  (:row-completed 3)
  (:scan-completed 4))

(cffi:defcstruct (quantization-table :conc-name quantization-table-)
  (values :uint16 :count #.DCT-SIZE-2)
  (sent-p :boolean))

(cffi:defcstruct (huffman-table :conc-name huffman-table-)
  (bits :uint8 :count 17)
  (values :uint8 :count 256)
  (sent-p :boolean))

(cffi:defcstruct (component-info :conc-name component-info-)
  (component-id :int)
  (component-index :int)
  (horizontal-sampling-factor :int)
  (vertical-sampling-factor :int)
  (quantization-table-no :int)
  (dc-entropy-table :int)
  (ac-entropy-table :int)
  (width-in-blocks :uint)
  (height-in-blocks :uint)
  (dct-horizontal-scaled-size :int)
  (dct-vertical-scaled-size :int)
  (downsampled-width :uint)
  (downsampled-height :uint)
  (component-needed-p :boolean)
  (mcu-width :int)
  (mcu-height :int)
  (mcu-blocks :int)
  (mcu-sample-width :int)
  (last-column-width :int)
  (last-row-height :int)
  (quantization-table :pointer)
  (dct-table :pointer))

(cffi:defcstruct (scan-info :conc-name scan-info-)
  (components-in-scan :int)
  (component-index :int :count #.MAX-SCAN-COMPONENTS)
  (ss :int)
  (se :int)
  (ah :int)
  (al :int))

(cffi:defcstruct (marker :conc-name marker-)
  (next :pointer)
  (marker :uint8)
  (original-length :uint)
  (data-length :uint)
  (data :pointer))

(cffi:defcstruct (compressor :conc-name compressor-)
  (error-manager :pointer)
  (memory-manager :pointer)
  (progress-manager :pointer)
  (client-data :pointer)
  (decompressor-p :boolean)
  (global-state :int)

  (destination-manager :pointer)
  (image-width :uint)
  (image-height :uint)
  (input-components :int)
  (input-color-space color-space)
  (input-gamma :double)
  (scale-number :uint)
  (scale-denominator :uint)
  (scaled-width :uint)
  (scaled-height :uint)
  (data-precision :int)
  (component-count :int)
  (color-space color-space)
  (component-info :pointer)
  (quantization-tables :pointer :count #.QUANTIZATION-TABLES)
  (q-scale-factor :int :count #.QUANTIZATION-TABLES)
  (dc-tables :pointer :count #.HUFFMAN-TABLES)
  (ac-tables :pointer :count #.HUFFMAN-TABLES)
  (arith-dc-l :uint8 :count #.ARITH-CODING-TABLES)
  (arith-dc-u :uint8 :count #.ARITH-CODING-TABLES)
  (arith-dc-k :uint8 :count #.ARITH-CODING-TABLES)
  (scan-conut :int)
  (scan-info :pointer)
  (raw-data-in :boolean)
  (arith-code :boolean)
  (optimize-coding :boolean)
  (ccir601-sampling :boolean)
  (fancy-downsampling :boolean)
  (smoothing-factor :int)
  (dct-method dct-method)
  (restrat-interval :uint)
  (restart-in-rows :int)
  (write-jfif-header :boolean)
  (jfif-major-version :uint8)
  (jfif-minor-version :uint8)
  (density-unit :uint8)
  (x-density :uint16)
  (y-density :uint16)
  (write-adobe-marker :boolean)
  (next-scanline :uint)
  (progressive-mode :boolean)
  (max-horizontal-sampling-factor :int)
  (max-vertical-sampling-factor :int)
  (min-dct-horizontal-scaled-size :int)
  (min-dct-vertical-scaled-size :int)
  (total-imcu-rows :uint)
  (components-in-scan :int)
  (current-component-info :pointer :count #.MAX-SCAN-COMPONENTS)
  (mcus-per-row :uint)
  (mcu-rows-in-scan :uint)
  (blocks-in-mcu :int)
  (mcu-membership :int :count #.MAX-BLOCKS-IN-MCU)
  (ss :int)
  (se :int)
  (ah :int)
  (al :int)
  (block-size :int)
  (natural-order :pointer)
  (lim-se :int)
  (compression-master :pointer)
  (c-main-controller :pointer)
  (c-prep-controller :pointer)
  (c-coef-controller :pointer)
  (marker-writer :pointer)
  (color-converter :pointer)
  (downsampler :pointer)
  (forward-dct :pointer)
  (entropy-encoder :pointer)
  (script-space :pointer)
  (script-space-size :int))

(cffi:defcstruct (decompressor :conc-name decompressor-)
  (error-manager :pointer)
  (memory-manager :pointer)
  (progress-manager :pointer)
  (client-data :pointer)
  (decompressor-p :boolean)
  (global-state :int)

  (source-manager :pointer)
  (image-width :uint)
  (image-height :uint)
  (components :int)
  (color-space color-space)
  (output-color-space color-space)
  (scale-number :uint)
  (scale-denominator :uint)
  (output-gamma :double)
  (buffered-image :boolean)
  (raw-data-out :boolean)
  (dct-method dct-method)
  (fancy-upsampling :boolean)
  (block-smoothing :boolean)
  (quantize-colors :boolean)
  (dither-mode dither-mode)
  (two-pass-quantize :boolean)
  (enable-1-pass-quantization :boolean)
  (enable-external-quantization :boolean)
  (enable-2-pass-quantization :boolean)
  (output-width :uint)
  (output-height :uint)
  (output-color-components :int)
  (output-components :int)
  (recommended-outbuffer-height :int)
  (actual-number-of-colors :int)
  (colormap :pointer)
  (output-scanline :uint)
  (input-scan-number :int)
  (input-imcu-row :uint)
  (outptu-scan-number :int)
  (output-imcu-row :uint)
  (coef-bits :int :count #.DCT-SIZE-2)
  (quantization-tables :pointer :count #.QUANTIZATION-TABLES)
  (dc-huffman-tables :pointer :count #.HUFFMAN-TABLES)
  (ac-huffman-tables :pointer :count #.HUFFMAN-TABLES)
  (data-precision :int)
  (component-info :pointer)
  (baseline-p :boolean)
  (progressive-mode :boolean)
  (arith-code :boolean)
  (arith-dc-l :uint8 :count #.ARITH-CODING-TABLES)
  (arith-dc-u :uint8 :count #.ARITH-CODING-TABLES)
  (arith-dc-k :uint8 :count #.ARITH-CODING-TABLES)
  (restart-interval :uint)
  (saw-jfif-marker :boolean)
  (jfif-major-version :uint8)
  (jfif-minor-version :uint8)
  (density-unit :uint8)
  (x-density :uint16)
  (y-density :uint16)
  (saw-adobe-marker :boolean)
  (adobe-transform :uint8)
  (ccir601-sampling :boolean)
  (marker-list :pointer)
  (max-horizontal-sampling-factor :int)
  (max-vertical-sampling-factor :int)
  (min-dct-horizontal-scaled-size :int)
  (min-dct-vertical-scaled-size :int)
  (total-imcu-rows :uint)
  (sample-range-limit :pointer)
  (components-in-scan :int)
  (current-component-info :pointer :count #.MAX-SCAN-COMPONENTS)
  (mcus-per-row :uint)
  (mcu-rows-in-scan :uint)
  (blocks-in-mcu :int)
  (mcu-membership :int :count #.MAX-BLOCKS-IN-MCU)
  (ss :int)
  (se :int)
  (ah :int)
  (al :int)
  (block-size :int)
  (natural-order :pointer)
  (lim-se :int)
  (unread-marker :int)
  (decompression-master :pointer)
  (d-main-controller :pointer)
  (d-coef-controller :pointer)
  (d-post-controller :pointer)
  (input-controller :pointer)
  (marker-reader :pointer)
  (entropy-decoder :pointer)
  (inverse-dct :pointer)
  (upsampler :pointer)
  (color-deconverter :pointer)
  (color-quantizer :pointer))

(cffi:defcstruct (error-manager :conc-name error-manager-)
  (error-exit :pointer)
  (emit-message :pointer)
  (output-message :pointer)
  (format-message :pointer)
  (reset-error-manager :pointer)
  (message-code :int)
  (parameters :int :count 8)
  (trace-level :int)
  (warning-count :long)
  (message-table :pointer)
  (last-message :int)
  (addon-message-table :pointer)
  (first-addon-message :int)
  (last-addon-message :int))

(cffi:defcstruct (progress-manager :conc-name progress-manager-)
  (monitor :pointer)
  (pass-counter :long)
  (pass-limit :long)
  (completed-passes :int)
  (total-passes :int))

(cffi:defcstruct (destination-manager :conc-name destination-manager-)
  (next-output-byte :pointer)
  (free-in-buffer :size)
  (init-destination :pointer)
  (empty-output-buffer :pointer)
  (term-destination :pointer))

(cffi:defcstruct (source-manager :conc-name source-manager-)
  (next-input-byte :pointer)
  (bytes-in-buffer :size)
  (init-source :pointer)
  (fill-input-buffer :pointer)
  (skip-input-data :pointer)
  (resync-to-restart :pointer)
  (term-source :pointer))

(cffi:defcstruct (memory-manager :conc-name memory-manager-)
  (alloc-small :pointer)
  (alloc-large :pointer)
  (alloc-sarray :pointer)
  (alloc-barray :pointer)
  (request-virt-sarray :pointer)
  (request-virt-barray :pointer)
  (realize-virt-arrays :pointer)
  (access-virt-sarray :pointer)
  (access-virt-barray :pointer)
  (free-pool :pointer)
  (self-destruct :pointer)
  (max-memory-to-use :long)
  (max-alloc-chunk :long))

(cffi:defcfun ("jpeg_std_error" error-manager) :pointer
  (manager :pointer))

(cffi:defcfun ("jpeg_CreateCompress" make-compress) :void
  (info :pointer)
  (version :int)
  (struct-size :size))

(cffi:defcfun ("jpeg_CreateDecompress" make-decompress) :void
  (info :pointer)
  (version :int)
  (struct-size :size))

(cffi:defcfun ("jpeg_destroy_compress" destroy-compress) :void
  (info :pointer))

(cffi:defcfun ("jpeg_destroy_decompress" destroy-decompress) :void
  (info :pointer))

(cffi:defcfun ("jpeg_stdio_dest" file-dest) :void
  (info :pointer)
  (outfile :pointer))

(cffi:defcfun ("jpeg_stdio_src" file-src) :void
  (info :pointer)
  (infile :pointer))

(cffi:defcfun ("jpeg_mem_dest" memory-dest) :void
  (info :pointer)
  (buffer :pointer)
  (size :ulong))

(cffi:defcfun ("jpeg_mem_src" memory-src) :void
  (info :pointer)
  (buffer :pointer)
  (size :ulong))

(cffi:defcfun ("jpeg_set_defaults" set-defaults) :void
  (info :pointer))

(cffi:defcfun ("jpeg_set_colorspace" set-color-space) :void
  (info :pointer)
  (color-space color-space))

(cffi:defcfun ("jpeg_default_colorspace" default-color-space) :void
  (info :pointer))

(cffi:defcfun ("jpeg_set_quality" set-quality) :void
  (info :pointer)
  (quality :int)
  (force-baseline :boolean))

(cffi:defcfun ("jpeg_set_linear_quality" set-linear-quality) :void
  (info :pointer)
  (scale-factor :int)
  (force-baseline :boolean))

(cffi:defcfun ("jpeg_default_qtables" default-quantization-tables) :void
  (info :pointer))

(cffi:defcfun ("jpeg_add_quant_table" add-quantization-table) :void
  (info :pointer)
  (which-table :int)
  (basic-table :pointer)
  (scale-factor :int)
  (force-baseline :boolean))

(cffi:defcfun ("jpeg_quality_scaling" quality-scaling) :int
  (quality :int))

(cffi:defcfun ("jpeg_enable_lossless" enable-lossless) :void
  (info :pointer)
  (predictor-selection-value :int)
  (point-transform :int))

(cffi:defcfun ("jpeg_simple_progression" simple-progression) :void
  (info :pointer))

(cffi:defcfun ("jpeg_suppress_tables" suppress-tables) :void
  (info :pointer)
  (suppress-p :boolean))

(cffi:defcfun ("jpeg_alloc_quant_table" make-quantization-table) :pointer
  (info :pointer))

(cffi:defcfun ("jpeg_alloc_huff_table" make-huffman-table) :pointer
  (info :pointer))

(cffi:defcfun ("jpeg_start_compress" start-compress) :void
  (info :pointer)
  (write-all-tables :boolean))

(cffi:defcfun ("jpeg_write_scanlines" write-scanlines) :uint
  (info :pointer)
  (scanlines :pointer)
  (line-count :uint))

(cffi:defcfun ("jpeg12_write_scanlines" write-scanlines/12) :uint
  (info :pointer)
  (scanlines :pointer)
  (line-count :uint))

(cffi:defcfun ("jpeg16_write_scanlines" write-scanlines/16) :uint
  (info :pointer)
  (scanlines :pointer)
  (line-count :uint))

(cffi:defcfun ("jpeg_finish_compress" finish-compress) :void
  (info :pointer))

(cffi:defcfun ("jpeg_calc_jpeg_dimensions" calculate-jpeg-dimensions) :void
  (info :pointer))

(cffi:defcfun ("jpeg_write_raw_data" write-raw-data) :uint
  (info :pointer)
  (data :pointer)
  (line-count :uint))

(cffi:defcfun ("jpeg12_write_raw_data" write-raw-data/12) :uint
  (info :pointer)
  (data :pointer)
  (line-count :uint))

(cffi:defcfun ("jpeg_write_marker" write-marker) :void
  (info :pointer)
  (marker :int)
  (data :pointer)
  (length :uint))

(cffi:defcfun ("jpeg_write_m_header" write-m-header) :void
  (info :pointer)
  (marker :int)
  (length :uint))

(cffi:defcfun ("jpeg_write_m_byte" write-m-byte) :void
  (info :pointer)
  (value :int))

(cffi:defcfun ("jpeg_write_tables" write-tables) :void
  (info :pointer))

(cffi:defcfun ("jpeg_write_icc_profile" write-icc-profile) :void
  (info :pointer)
  (data :pointer)
  (length :uint))

(cffi:defcfun ("jpeg_read_header" read-header) read-header-return
  (info :pointer)
  (require-image :boolean))

(cffi:defcfun ("jpeg_start_decompress" start-decompress) :boolean
  (info :pointer))

(cffi:defcfun ("jpeg_read_scanlines" read-scanlines) :uint
  (info :pointer)
  (scanlines :pointer)
  (max-lines :uint))

(cffi:defcfun ("jpeg12_read_scanlines" read-scanlines/12) :uint
  (info :pointer)
  (scanlines :pointer)
  (max-lines :uint))

(cffi:defcfun ("jpeg16_read_scanlines" read-scanlines/16) :uint
  (info :pointer)
  (scanlines :pointer)
  (max-lines :uint))

(cffi:defcfun ("jpeg_skip_scanlines" skip-scanlines) :uint
  (info :pointer)
  (line-count :uint))

(cffi:defcfun ("jpeg12_skip_scanlines" skip-scanlines/12) :uint
  (info :pointer)
  (line-count :uint))

(cffi:defcfun ("jpeg_crop_scanline" crop-scanline) :void
  (info :pointer)
  (x-offset :pointer)
  (width :pointer))

(cffi:defcfun ("jpeg12_crop_scanline" crop-scanline/12) :void
  (info :pointer)
  (x-offset :pointer)
  (width :pointer))

(cffi:defcfun ("jpeg_finish_decompress" finish-decompress) :boolean
  (info :pointer))

(cffi:defcfun ("jpeg_read_raw_data" read-raw-data) :void
  (info :pointer)
  (data :pointer)
  (max-lines :uint))

(cffi:defcfun ("jpeg12_read_raw_data" read-raw-data/12) :void
  (info :pointer)
  (data :pointer)
  (max-lines :uint))

(cffi:defcfun ("jpeg_has_multiple_scans" multiple-scans-p) :boolean
  (info :pointer))

(cffi:defcfun ("jpeg_start_output" output-started-p) :boolean
  (info :pointer)
  (scan-number :int))

(cffi:defcfun ("jpeg_finish_output" output-finished-p) :boolean
  (info :pointer))

(cffi:defcfun ("jpeg_input_complete" input-complete-p) :boolean
  (info :pointer))

(cffi:defcfun ("jpeg_new_colormap" new-colormap) :void
  (info :pointer))

(cffi:defcfun ("jpeg_consume_input" consume-input) consume-input-return
  (info :pointer))

(cffi:defcfun ("jpeg_core_output_dimensions" core-output-dimensions) :void
  (info :pointer))

(cffi:defcfun ("jpeg_calc_output_dimensions" calculate-output-dimensions) :void
  (info :pointer))

(cffi:defcfun ("jpeg_save_markers" save-markers) :void
  (info :pointer)
  (marker-code :int)
  (length-limit :uint))

(cffi:defcfun ("jpeg_set_marker_processor" set-marker-processor) :void
  (info :pointer)
  (marker-mode :int)
  (routine :pointer))

(cffi:defcfun ("jpeg_read_coefficients" read-coefficients) :pointer
  (info :pointer))

(cffi:defcfun ("jpeg_write_coefficients" write-coefficients) :void
  (info :pointer)
  (coefficients :pointer))

(cffi:defcfun ("jpeg_copy_critical_parameters" copy-critical-parameters) :void
  (info :pointer)
  (dst :pointer))

(cffi:defcfun ("jpeg_abort_compress" abort-compress) :void
  (info :pointer))

(cffi:defcfun ("jpeg_abort_decompress" abort-decompress) :void
  (info :pointer))

(cffi:defcfun ("jpeg_abort" abort) :void
  (info :pointer))

(cffi:defcfun ("jpeg_destroy" destroy) :void
  (info :pointer))

(cffi:defcfun ("jpeg_resync_to_restart" resync-to-restart) :boolean
  (info :pointer)
  (desired :int))

(cffi:defcfun ("jpeg_read_icc_profile" read-icc-profile) :boolean
  (info :pointer)
  (icc-data :pointer)
  (data-length :pointer))

(in-package #:org.shirakumo.fraf.turbojpeg)

(docs:define-docs
  (type jpeg-condition
    "Supertype for all JPEG related conditions.

See JPEG
See MESSAGE")
  
  (function jpeg
    "Returns the JPEG object involved.

See JPEG (type)
See JPEG-CONDITION (type)")
  
  (function message
    "Returns the message about the condition.

See JPEG-CONDITION (type)")
  
  (type jpeg-error
    "Representation of a fatal error in the library.

See JPEG-CONDITION (type)")
  
  (type jpeg-warning
    "Representation of a warning in the library.

See JPEG-CONDITION (type)")
  
  (function init
    "Initialises the foreign library.

It is safe to call this multiple times.")
  
  (type jpeg
    "Wrapper around a turbojpeg handle.

Creating an instance will automatically call INIT for you.

You should not create instances of this class directly, but instead
create instances of the subclasses instead:

  COMPRESSOR
  DECOMPRESSOR
  TRANSFORMER

All of them share the following initargs:

  STOP-ON-WARNING
  BOTTOM-UP
  PROGRESSIVE
  ARITHMETIC
  HANDLE

See INIT
See HANDLE
See FREE")
  
  (function handle
    "Accesses the pointer to the underlying C object.

You can use this with any functions in the
ORG.SHIRAKUMO.FRAF.TURBOJPEG.CFFI package.

See JPEG (type)")
  
  (function free
    "Frees the object if it is allocated.

It is safe to call this multiple times.

See JPEG (type)")
  
  (type compressor
    "Representation of a jpeg compression object.

In addition to the shared initargs, you can pass the following:

  NO-REALLOC
  FAST-DCT
  OPTIMIZE
  LOSSLESS
  QUALITY
  SUBSAMPLING
  COLOR-SPACE
  RESTART-BLOCKS
  RESTART-ROWS
  X-DENSITY
  Y-DENSITY

See JPEG (type)
See SAVE-IMAGE")
  
  (function save-image
    "Save a JPEG image and encode it to from memory to file or memory.

DESTINATION may be one of the following:

  PATHNAME
  VECTOR
  CFFI:FOREIGN-POINTER
    You must also pass the SIZE keyword argument
  NIL
    A buffer of the appropriate size will be allocated and returned
    for you. You must call FREE on it when you are done with it.
size of the buffer in octets
SOURCE may be one of the following:

  VECTOR
  CFFI:FOREIGN-POINTER

Returns the following values:

  The pathname of the file if DESTINATION is a pathname. Otherwise:
  The buffer as a CFFI:FOREIGN-POINTER
  The number of octets written to the buffer

JPEG may be T, in which case a COMPRESSOR is managed for the duration
of the transformation.

See COMPRESSOR (type)")
  
  (type decompressor
    "Representation of a jpeg decompression object.

In addition to the shared initargs, you can pass the following:

  FAST-DCT
  FAST-UPSAMPLE
  OPTIMIZE
  LOSSLESS
  SUBSAMPLING
  COLOR-SPACE
  SCAN-LIMIT
  X-DENSITY
  Y-DENSITY

See JPEG (type)
See LOAD-IMAGE")
  
  (function load-image
    "Load a JPEG image and decode it into memory from file or memory.

SOURCE may be one of the following:

  PATHNAME
  VECTOR
  CFFI:FOREIGN-POINTER
    You must also pass the SIZE keyword argument

Returns the following values:

  The buffer as a CFFI:FOREIGN-POINTER or a VECTOR
    if :BUFFER is a VECTOR or :VECTOR
  The width
  The height
  The pixel-format
  The size of the buffer in octets

You may pass your own buffer via the BUFFER keyword argument. If no
buffer is passed, a new one will be allocated for you and you must
call FREE on it when you are done. If BUFFER is a VECTOR, the vector
will be resized to fit *at least* the number of octets necessary, via
ADJUST-ARRAY and the result of that is returned. If BUFFER is the
keyword :VECTOR, a fresh vector that fits exactly to the image data is
returned.

JPEG may be T, in which case a DECOMPRESSOR is managed for the
duration of the transformation.

See DECOMPRESSOR (type)")
  
  (type transformer
    "Representation of a jpeg transformation object.

In addition to the shared initargs, you can pass the following:

  NO-REALLOC
  SUBSAMPLING
  SCAN-LIMIT

See JPEG (type)
See TRANSFORM-IMAGE")
  
  (function transform-image
    "Transform a JPEG image in-place without decoding it.

Bouth SOURCE and DESTINATION may be one of the following:

  VECTOR
  PATHNAME
  CFFI:FOREIGN-POINTER
    You must also pass the respective SIZE keyword argument.

DESTINATION may also be NIL, in which case an appropriately sized
buffer is allocated for you. You must call FREE on it when you are
done with it.

The OPERATION may be one of the following:

  :NONE
  :HORIZONTAL-FLIP
  :VERTICAL-FLIP
  :TRANSPOSE
  :TRANSVERSE
  :ROTATE-90
  :ROTATE-180
  :ROTATE-270

JPEG may be T, in which case a TRANSFORMER is managed for the duration
of the transformation.

Returns the following values:

  The pathname, if DESTINATION was a pathname. Otherwise:
  The buffer as a CFFI:FOREIGN-POINTER
  The size of the buffer in octets

See TRANSFORMER (type)"))

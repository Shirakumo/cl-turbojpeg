ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
OS = misc
ARCH = misc
EXT = so

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	CFLAGS += -static-libgcc -include "$(ROOT_DIR)/glibcver/glibc_2.9_x64.h"
	OS = lin
endif
ifeq ($(UNAME_S),Darwin)
	OS = mac
	EXT = dylib
endif
ifneq ($(filter MINGW%,$(UNAME_S)),)
	OS = win
	EXT = dll
	CMAKEFLAGS += -G "MSYS Makefiles"
endif

UNAME_M := $(shell uname -m)
ifeq ($(UNAME_M),x86_64)
	ARCH = amd64
endif
ifneq ($(filter %86,$(UNAME_M)),)
	ARCH = i686
endif
ifneq ($(filter arm%,$(UNAME_M)),)
	ARCH = arm64
endif

all:
	mkdir -p build ../static
	cmake -B build/ -DCMAKE_BUILD_TYPE=ReleaseWithDebug -DENABLE_STATIC=OFF -DREQUIRE_SIMD=ON -DWITH_JPEG8=ON -DCMAKE_C_FLAGS="$(CFLAGS)" $(CMAKEFLAGS)
	$(MAKE) -C build
	cp build/libjpeg*.$(EXT) ../static/libjpeg-$(OS)-$(ARCH).$(EXT)
	cp build/libturbojpeg*.$(EXT) ../static/libturbojpeg-$(OS)-$(ARCH).$(EXT)

ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
PREFIX ?= build

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	OS ?= lin
endif
ifeq ($(UNAME_S),Darwin)
	OS ?= mac
endif
ifneq ($(filter MINGW%,$(UNAME_S)),)
	OS ?= win
	CMAKEFLAGS += -G "MSYS Makefiles"
endif

ifeq ($(ARCH),)
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
endif

ifeq ($(OS),lin)
	EXT ?= so
ifeq ($(ARCH),amd64)
	CFLAGS += -static-libgcc -include "$(ROOT_DIR)/glibcver/glibc_2.9_x64.h"
endif
ifeq ($(ARCH),i686)
	CFLAGS += -static-libgcc -include "$(ROOT_DIR)/glibcver/glibc_2.9_x86.h"
endif
endif
ifeq ($(OS),mac)
	EXT ?= dylib
endif
ifeq ($(OS),win)
	EXT ?= dll
endif

OS ?= misc
ARCH ?= misc
EXT ?= so

native:
	mkdir -p $(PREFIX) ../static
	cmake -B $(PREFIX)/ -DCMAKE_BUILD_TYPE=ReleaseWithDebug -DENABLE_STATIC=OFF -DREQUIRE_SIMD=ON -DWITH_JPEG8=ON -DCMAKE_C_FLAGS="$(CFLAGS)" $(CMAKEFLAGS)
	$(MAKE) -C $(PREFIX)
	cp $(PREFIX)/libjpeg*.$(EXT) ../static/libjpeg-$(OS)-$(ARCH).$(EXT)
	cp $(PREFIX)/libturbojpeg*.$(EXT) ../static/libturbojpeg-$(OS)-$(ARCH).$(EXT)

win-amd64:
	mkdir -p $(PREFIX)-$@
	$(MAKE) native PREFIX=$(PREFIX)-$@ OS=win ARCH=amd64 CMAKEFLAGS=-DCMAKE_TOOLCHAIN_FILE=cmakescripts/x86_64-w64-mingw32-toolchain.cmake

win-i686:
	mkdir -p $(PREFIX)-$@
	$(MAKE) native PREFIX=$(PREFIX)-$@ OS=win ARCH=i686 CMAKEFLAGS=-DCMAKE_TOOLCHAIN_FILE=cmakescripts/i686-w64-mingw32-toolchain.cmake

lin-aarch64:
	mkdir -p $(PREFIX)-$@
	$(MAKE) native PREFIX=$(PREFIX)-$@ OS=lin ARCH=arm64 CMAKEFLAGS=-DCMAKE_TOOLCHAIN_FILE=cmakescripts/aarch64-gcc-toolchain.cmake

android-amd64:
	mkdir -p $(PREFIX)-$@
	$(MAKE) native PREFIX=$(PREFIX)-$@ OS=android ARCH=amd64 CMAKEFLAGS=-DCMAKE_TOOLCHAIN_FILE=cmakescripts/amd64-android-toolchain.cmake

android-arm7a:
	mkdir -p $(PREFIX)-$@
	$(MAKE) native PREFIX=$(PREFIX)-$@ OS=android ARCH=arm7a CMAKEFLAGS=-DCMAKE_TOOLCHAIN_FILE=cmakescripts/arm7a-android-toolchain.cmake

android-aarch64:
	mkdir -p $(PREFIX)-$@
	$(MAKE) native PREFIX=$(PREFIX)-$@ OS=android ARCH=aarch64 CMAKEFLAGS=-DCMAKE_TOOLCHAIN_FILE=cmakescripts/aarch64-android-toolchain.cmake

all:
	$(MAKE) build
	$(MAKE) win-amd64
	$(MAKE) win-i686
	$(MAKE) lin-aarch64
	$(MAKE) android-aarch64
	$(MAKE) android-arm7a
	$(MAKE) android-amd64

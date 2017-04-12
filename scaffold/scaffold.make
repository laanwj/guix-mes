TARGET:=m
C_FILES:=$(DIR)/m.c
DEFINES:=POSIX=1
INCLUDES:=libc
include make/bin.make

TARGET:=m
EXPECT:=255
include make/check.make

TARGET:=hello
C_FILES:=$(DIR)/hello.c
DEFINES:=POSIX=1
INCLUDES:=libc
include make/bin.make

TARGET:=hello
EXPECT:=42
include make/check.make

TARGET:=t
C_FILES:=$(DIR)/t.c
DEFINES:=POSIX=1
INCLUDES:=libc
include make/bin.make

TARGET:=t
include make/check.make

TARGET:=m.mlibc
C_FILES:=$(DIR)/m.c
INCLUDES:=libc
C_FLAGS:=-nostdinc
LD_FLAGS:=-nostdlib
CROSS:=$(CC32:%gcc=%)
include make/bin.make

TARGET:=m.mlibc
EXPECT:=255
include make/check.make

TARGET:=hello.mlibc
C_FILES:=$(DIR)/hello.c
INCLUDES:=libc
C_FLAGS:=-nostdinc
LD_FLAGS:=-nostdlib
CROSS:=$(CC32:%gcc=%)
include make/bin.make

TARGET:=hello.mlibc
EXPECT:=42
include make/check.make

TARGET:=micro-mes.mlibc
C_FILES:=$(DIR)/micro-mes.c
INCLUDES:=libc
C_FLAGS:=-nostdinc
LD_FLAGS:=-nostdlib
CROSS:=$(CC32:%gcc=%)
include make/bin.make

TEST:=micro-mes.mlibc-check
$(TEST): $(OUT)/micro-mes.mlibc
	$< 2 3; r=$$?; [ $$r = 3 ]
include make/check.make

TARGET:=tiny-mes.mlibc
C_FILES:=$(DIR)/tiny-mes.c
INCLUDES:=libc
C_FLAGS:=-nostdinc
LD_FLAGS:=-nostdlib
CROSS:=$(CC32:%gcc=%)
include make/bin.make

TARGET:=tiny-mes.mlibc
include make/check.make

TARGET:=cons-mes.mlibc
C_FILES:=$(DIR)/cons-mes.c
INCLUDES:=libc
C_FLAGS:=-nostdinc
LD_FLAGS:=-nostdlib
DEFINES:=VERSION='"$(VERSION)"'
CROSS:=$(CC32:%gcc=%)
include make/bin.make

TARGET:=cons-mes.mlibc
include make/check.make

TARGET:=t.mlibc
C_FILES:=$(DIR)/t.c
INCLUDES:=libc
C_FLAGS:=-nostdinc
LD_FLAGS:=-nostdlib
CROSS:=$(CC32:%gcc=%)
include make/bin.make

TARGET:=t.mlibc
include make/check.make

$(OUT)/mini-mes: $(SNARF.MES)

TARGET:=mini-mes.mlibc
C_FILES:=$(DIR)/mini-mes.c
DEFINES:=FIXED_PRIMITIVES=1 VERSION='"$(VERSION)"' PREFIX='"$(PREFIX)"'
INCLUDES:=libc src $(OUT)/src
C_FLAGS:=-nostdinc
LD_FLAGS:=-nostdlib
CROSS:=$(CC32:%gcc=%)
include make/bin.make

TEST:=mini-mes.mlibc-check
$(TEST): $(OUT)/mini-mes.mlibc
	echo 0 | $<
include make/check.make

# guile/mescc.scm

TARGET:=m.guile
C_FILES:=$(DIR)/m.c
include make/mescc-guile.make

TARGET:=m.guile
EXPECT:=255
include make/check.make

TARGET:=hello.guile
C_FILES:=$(DIR)/hello.c
include make/mescc-guile.make

TARGET:=hello.guile
EXPECT:=42
include make/check.make

TARGET:=micro-mes.guile
C_FILES:=$(DIR)/micro-mes.c
include make/mescc-guile.make

TEST:=micro-mes.guile-check
$(TEST): $(OUT)/micro-mes.guile
	$< 2 3; r=$$?; [ $$r = 3 ]
include make/check.make

$(OUT)/tiny-mes.mes: module/mes/tiny-0-32.mo
TARGET:=tiny-mes.guile
C_FILES:=$(DIR)/tiny-mes.c
include make/mescc-guile.make

TARGET:=tiny-mes.guile
include make/check.make

TARGET:=cons-mes.guile
C_FILES:=$(DIR)/cons-mes.c
include make/mescc-guile.make

TARGET:=cons-mes.guile
include make/check.make

TARGET:=t.guile
C_FILES:=$(DIR)/t.c
include make/mescc-guile.make

TARGET:=t.guile
include make/check.make

$(OUT)/mini-mes.guile: module/mes/read-0-32.mo
TARGET:=mini-mes.guile
C_FILES:=$(DIR)/mini-mes.c
include make/mescc-guile.make

TEST:=mini-mes.guile-check
$(TEST): $(OUT)/mini-mes.guile
	echo 0 | $<
include make/check.make

# scripts/mescc.mes

TARGET:=m.mes
C_FILES:=$(DIR)/m.c
include make/mescc-mes.make

TARGET:=m.mes
EXPECT:=255
include make/check.make

ifneq ($(SCAFFOLD),)
TARGET:=hello.mes
C_FILES:=$(DIR)/hello.c
include make/mescc-mes.make

TARGET:=hello.mes
EXPECT:=42
include make/check.make

TARGET:=micro-mes.mes
C_FILES:=$(DIR)/micro-mes.c
include make/mescc-mes.make

TEST:=micro-mes.mes-check
$(TEST): $(OUT)/micro-mes.mes
	$< 2 3; r=$$?; [ $$r = 3 ]
include make/check.make

$(OUT)/tiny-mes.mes: module/mes/tiny-0-32.mo
TARGET:=tiny-mes.mes
C_FILES:=$(DIR)/tiny-mes.c
include make/mescc-mes.make

TARGET:=tiny-mes.mes
include make/check.make

TARGET:=cons-mes.mes
C_FILES:=$(DIR)/cons-mes.c
include make/mescc-mes.make

TARGET:=cons-mes.mes
include make/check.make
endif # !SCAFFOLD

TARGET:=t.mes
C_FILES:=$(DIR)/t.c
include make/mescc-mes.make

TARGET:=t.mes
include make/check.make

ifneq ($(BOOTSTRAP),)
$(OUT)/mini-mes.mes: module/mes/read-0-32.mo
TARGET:=mini-mes.mes
C_FILES:=$(DIR)/mini-mes.c
include make/mescc-mes.make
endif

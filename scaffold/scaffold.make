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
include make/bin-mlibc.make

TARGET:=m.mlibc
EXPECT:=255
include make/check.make

TARGET:=hello.mlibc
C_FILES:=$(DIR)/hello.c
include make/bin-mlibc.make

TARGET:=hello.mlibc
EXPECT:=42
include make/check.make

TARGET:=micro-mes.mlibc
C_FILES:=$(DIR)/micro-mes.c
include make/bin-mlibc.make

TEST:=micro-mes.mlibc-check
$(TEST): $(OUT)/micro-mes.mlibc
	$< 2 3; r=$$?; [ $$r = 3 ]
include make/check.make

TARGET:=t.mlibc
C_FILES:=$(DIR)/t.c
include make/bin-mlibc.make

TARGET:=t.mlibc
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

TARGET:=t.guile
C_FILES:=$(DIR)/t.c
include make/mescc-guile.make

TARGET:=t.guile
include make/check.make

# scripts/mescc.mes
ifeq ($(MES_SKIP_MES),)
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
endif # !SCAFFOLD

TARGET:=t.mes
C_FILES:=$(DIR)/t.c
include make/mescc-mes.make

TARGET:=t.mes
include make/check.make
endif

ifneq ($(CC32),)
C_FLAGS:=-nostdinc -fno-builtin
LD_FLAGS:=-nostdlib
CROSS:=$(CC32:%gcc=%)

include make/bin.make
endif

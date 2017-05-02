C_FLAGS:=-nostdinc --include mstart.c -fno-builtin
LD_FLAGS:=-nostdlib
CROSS:=$(CC32:%gcc=%)

include make/bin.make

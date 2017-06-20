TINYCC_ROOT:=../tinycc
TINYCC_TEST2:=$(TINYCC_ROOT)/tests/tests2

TINYCC_TESTS:=\
 00_assignment\
 01_comment\
 02_printf\
 03_struct\
 04_for\
 05_array\
 06_case\
 07_function\
 08_while\
 09_do_while\
 10_pointer\
 11_precedence\
 12_hashdefine\
 13_integer_literals\
 14_if\
 15_recursion\
 16_nesting\
 17_enum\
 18_include\
 19_pointer_arithmetic\
 20_pointer_comparison\
 21_char_array\
 25_quicksort\
 29_array_address\
 33_ternary_op\
 35_sizeof\
 41_hashif\
 43_void_param\
 44_scoped_declarations\
 47_switch_return\
 48_nested_break\
 54_goto\
#

TINYCC_NYACC:=\
 26_character_constants\
#

TINYCC_FLOAT:=\
 22_floating_point\
 23_type_coercion\
 24_math_library\
 27_sizeof\
#

# Does not compile/run with GCC
TINYCC_GCC:=\
 30_hanoi\
 34_array_assignment\
#

TINYCC_REST:=\
 28_strings\
 31_args\
 32_led\
 36_array_initialisers\
 37_sprintf\
 38_multiple_array_index\
 39_typedef\
 40_stdio\
 42_function_pointer\
 45_empty_for\
 46_grep\
 49_bracket_evaluation\
 50_logical_second_arg\
 51_static\
 52_unnamed_enum\
 55_lshift_type\
#

define tinycc-test
ifneq ($(CC32),)
EXT:=mlibc
TARGET:=$(1).mlibc
C_FILES:=$$(TINYCC_TEST2)/$(1).c
INCLUDES:=$$(TINYCC_TEST2)
include make/bin-mlibc.make

EXT:=mlibc
TARGET:=$(1).mlibc
include make/check-tinycc.make
endif

EXT:=guile
TARGET:=$(1).guile
C_FILES:=$$(TINYCC_TEST2)/$(1).c
INCLUDES:=$$(TINYCC_TEST2)
include make/mescc-guile.make

EXT:=guile
TARGET:=$(1).guile
include make/check-tinycc.make
endef

ifeq ($(wildcard $(TINYCC_TEST2)/00_assignment.c),$(TINYCC_TEST2)/00_assignment.c)
$(foreach t,$(TINYCC_TESTS),$(eval $(call tinycc-test,$(t))))
else
tinycc-check:
	@echo skipping tinycc tests: $(TINYCC_TEST2)/00_assignment.c not available
endif

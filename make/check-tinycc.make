ifeq ($(TEST),)
TEST:=$(TARGET)-check
$(TEST): EXT:=$(EXT)
$(TEST): EXPECT:=$(EXPECT)
$(TEST): $(OUT)/$(TARGET)
ifeq ($(EXPECT),)
	$< - arg1 arg2 arg3 arg4 > $(<:.$(EXT)=.stdout)
else
	$<; r=$$?; [ $$r = $(EXPECT) ]
endif
	$(QUIETx)if diff -bu $(TINYCC_TEST2)/$(<F:.$(EXT)=.expect) $(<:.$(EXT)=.stdout); \
	then rm -f $(<:.$(EXT)=.stdout); \
	else exit 1; \
	fi
endif
CHECK+=$(TEST)
$(TEST): TEST:=$(TEST)
$(DIR)-check: $(TEST)
include make/reset.make

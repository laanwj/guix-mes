ifeq ($(TEST),)
TEST:=$(TARGET)-check
$(TEST): EXPECT:=$(EXPECT)
$(TEST): $(OUT)/$(TARGET)
ifeq ($(EXPECT),)
	$<
else
	$<; r=$$?; [ $$r = $(EXPECT) ]
endif
endif
CHECK+=$(TEST)
$(TEST): TEST:=$(TEST)
$(DIR)-check: $(TEST)
include make/reset.make

ifneq ($(CC),)
ifeq ($(TEST),)
TEST:=$(TARGET)-check
$(TEST): EXPECT:=$(EXPECT)
$(TEST): SHELL:=$(SHELL)
$(TEST): $(OUT)/$(TARGET)
	@export SHELL=$(SHELL)
ifeq ($(EXPECT),)
	$<
else
	$<; r=$$?; [ $$r = $(EXPECT) ]
endif
endif
CHECK+=$(TEST)
$(TEST): TEST:=$(TEST)
$(DIR)-check: $(TEST)
endif
include make/reset.make


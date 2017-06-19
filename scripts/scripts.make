ifneq ($(CC),)
CLEAN+=$(DIR)/mes

$(DIR)/mes: $(OUT)/mes
	ln -sf ../$< $@
endif

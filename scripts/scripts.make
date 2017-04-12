CLEAN+=$(DIR)/mes

$(DIR)/mes: $(OUT)/mes
	ln -sf ../$< $@

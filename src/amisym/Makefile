EBIN_DIR= ../../ebin
INCLUDE_DIR= ../../include

ERL_FLAGS= -pa $(EBIN_DIR) -I $(INCLUDE_DIR) -W -o $(EBIN_DIR)

$(EBIN_DIR)/%.beam:  %.erl
	@mkdir -p $(EBIN_DIR)
	erlc $(ERL_FLAGS) $<


OBJECTS= amisym_interp amisym_session \
		amisym_server amisym_sup \
		amisym_eventbus amisym_actions \
		amisym


all: ${OBJECTS:%=$(EBIN_DIR)/%.beam}
	@echo AmiSym Ready

run: all
	erl -pa $(EBIN_DIR) -s amisym start 

clean:
	@rm -f $(EBIN_DIR)/*.beam
	@rm -f $(EBIN_DIR)/erl_crash.dump

EBIN_DIR= ../../ebin
INCLUDE_DIR= ../../include

ERL_FLAGS= -pa $(EBIN_DIR) -I $(INCLUDE_DIR) -W -o $(EBIN_DIR)

$(EBIN_DIR)/%.beam:  %.erl
	@mkdir -p $(EBIN_DIR)
	erlc $(ERL_FLAGS) $<


OBJECTS= gen_listener_tcp amisym_interp amisym_session \
		amisym_server amisym_sup \
		amisym_eventbus amisym_actions \
		amisym amisym_client_sup \
		amisym_app


all: ${OBJECTS:%=$(EBIN_DIR)/%.beam}
	@cp amisym.app $(EBIN_DIR)
	@echo AmiSym Ready

run: all
	erl -pa $(EBIN_DIR) -s amisym_app start

clean:
	@rm -f $(EBIN_DIR)/*.beam
	@rm -f $(EBIN_DIR)/erl_crash.dump

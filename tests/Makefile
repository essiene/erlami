EBIN_DIR= ../ebin
INCLUDE_DIR= ../include

ERL_FLAGS= -pa $(EBIN_DIR) -I $(INCLUDE_DIR) -W -o $(EBIN_DIR)

$(EBIN_DIR)/%.beam: %.erl
	erlc $(ERL_FLAGS) $<


OBJECTS=test_util test_amilist \
		test_messaging  test_amievent_manager \
		test_amisym_eventbus test_amisym_server \
		test_amisym test_ami


test: compile
	erl $(ERL_FLAGS) -noshell -s amisym $(foreach t, $(OBJECTS), -s $(t) test) -s amisym stop -s init stop


compile: ${OBJECTS:%=$(EBIN_DIR)/%.beam}
	@echo compile done

clean:
	@rm -f *.beam
	@rm -f erl_crash.dump

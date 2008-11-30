EBIN_DIR= ../ebin
INCLUDE_DIR= ../include

ERL_FLAGS= -pa $(EBIN_DIR) -I $(INCLUDE_DIR) -W

%.beam: %.erl
	erlc $(ERL_FLAGS) $<


OBJECTS=test_util test_amilist \
		test_messaging  test_amisym_eventbus \
		test_amievent_manager test_amisym \
		test_ami


test: compile
	erl $(ERL_FLAGS) -noshell $(foreach t, $(OBJECTS), -s $(t) test) -s init stop


compile: ${OBJECTS:%=%.beam}
	@echo 

clean:
	@rm -f *.beam
	@rm -f erl_crash.dump

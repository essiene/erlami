EBIN_DIR= ../ebin
INCLUDE_DIR= ../include

ERL_FLAGS= -pa $(EBIN_DIR) -I $(INCLUDE_DIR) -W

%.beam: %.erl
	erlc $(ERL_FLAGS) $<


OBJECTS=test_util test_amilist \
		test_messaging  \
		test_amisym test_ami


test: compile
	erl $(ERL_FLAGS) -noshell -s test_util test -s init stop
	erl $(ERL_FLAGS) -noshell -s test_amilist test -s init stop
	erl $(ERL_FLAGS) -noshell -s test_messaging test -s init stop

	erl $(ERL_FLAGS) -noshell -s test_amisym test -s init stop
	erl $(ERL_FLAGS) -noshell -s test_ami test -s init stop


compile: ${OBJECTS:%=%.beam}
	@echo 

clean:
	@rm -f *.beam
	@rm -f erl_crash.dump

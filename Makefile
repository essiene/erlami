.SUFFIXES: .erl .beam


.erl.beam: 
	erlc -W $<


OBJECTS=util.beam \
		rawmsg.beam \
		protocol.beam \


TESTS=test_util.beam \
	  test_rawmsg.beam


all: $(OBJECTS)
	@echo All Done


clean:
	rm -f *.beam
	rm -f erl_crash.dump


test: $(OBJECTS) $(TESTS)
	erl -noshell  -s test_util test -s init stop
	erl -noshell  -s test_rawmsg test -s init stop


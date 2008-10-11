.SUFFIXES: .erl .beam


.erl.beam: 
	erlc -W $<


OBJECTS=rawmsg.beam


TESTS=test_rawmsg.beam


all: $(OBJECTS)
	@echo All Done


clean:
	rm -f *.beam
	rm -f erl_crash.dump


test: $(OBJECTS) $(TESTS)
	erl -noshell  -s test_rawmsg test -s init stop


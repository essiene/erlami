.SUFFIXES: .erl .beam


.erl.beam: 
	erlc -W $<


OBJECTS=util.beam \
		messaging.beam \
		amilist.beam \
		protocol.beam \


TESTS=test_util.beam \
	  test_messaging.beam


all: $(OBJECTS)
	@echo All Done


clean:
	rm -f *.beam
	rm -f erl_crash.dump


test: $(OBJECTS) $(TESTS)
	erl -noshell  -s test_util test -s init stop
	erl -noshell  -s test_messaging test -s init stop


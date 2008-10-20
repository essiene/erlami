EBIN_DIR= ../ebin
INCLUDE_DIR= ../include

ERL_FLAGS= -pa $(EBIN_DIR) -I $(INCLUDE_DIR) -W -o $(EBIN_DIR)

$(EBIN_DIR)/%.beam:  %.erl
	@mkdir -p $(EBIN_DIR)
	erlc $(ERL_FLAGS) $<


OBJECTS=util amilist \
		messaging amitcp \
		interp


all: ${OBJECTS:%=$(EBIN_DIR)/%.beam}
	@echo AMI Core Library ready

clean:
	@rm -f $(EBIN_DIR)/*.beam
	@rm -f $(EBIN_DIR)/erl_crash.dump

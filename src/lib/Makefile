EBIN_DIR= ../../ebin
INCLUDE_DIR= ../../include

ERL_FLAGS= -pa $(EBIN_DIR) -I $(INCLUDE_DIR) -W -o $(EBIN_DIR)

$(EBIN_DIR)/%.beam:  %.erl
	@mkdir -p $(EBIN_DIR)
	erlc $(ERL_FLAGS) $<

$(EBIN_DIR)/%.app:  %.app
	@mkdir -p $(EBIN_DIR)
	cp $< $@


OBJECTS=util amilist \
		ami_socket \
		messaging amitcp \
		interp ami_interp

APPS=erlami


all: ${OBJECTS:%=$(EBIN_DIR)/%.beam} ${APPS:%=$(EBIN_DIR)/%.app} 
	@echo AMI Core Library ready

clean:
	@rm -f $(EBIN_DIR)/*.beam
	@rm -f $(EBIN_DIR)/erl_crash.dump

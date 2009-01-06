all: 
	@cd src; make

test: all
	@cd tests; make

runsym: all
	@cd src;make runsym

clean:
	@cd src; make clean
	@rm -rf ebin

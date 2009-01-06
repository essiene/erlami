all: 
	@cd lib; make
	@cd amisym; make
	@cd client; make

test: all
	@cd tests; make

runsym: all
	@cd amisym; make run

clean:
	@cd lib; make clean
	@cd amisym; make clean
	@cd client; make clean
	@rm -rf ebin

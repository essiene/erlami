all: 
	cd lib; make
	cd amisym; make

test: all
	cd tests; make

clean:
	cd lib; make clean
	cd amisym; make clean
	cd tests; make clean
	rm -rf ebin




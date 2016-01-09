all:
	@mkdir -p build
	@-mv build/*.o build/*.hi .
	@ghc -Wall main.hs
	@-mv *.o *.hi build

edit:
	vim -p *.hs

clean:
	-rm build/*

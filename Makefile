DSTBIN=/usr/local/bin
DSTLIB=/usr/local/lib/lmlc
DSTMAN=/usr/local/man/man1

BIN=sparc-solaris2.5
DIRS=bin lib hbc_library1.3 hlib1.3

none:
	@echo No target specified.

install:
	cp bin-$(BIN)/exe/* $(DSTBIN)
	-mkdir $(DSTLIB)
	cd bin-$(BIN); cp -r $(DIRS) $(DSTLIB)
	-ranlib $(DSTLIB)/*/*.a
	cp man/* $(DSTMAN)

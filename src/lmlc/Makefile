DSTBIN = /usr/local/bin
DST = /usr/local/lib/lmlc
CFLAGS = 
RCFLAGS = $(STATICFLAG) $(CFLAGS) $(SYSTEM) $(GCCLINK)
CC = sh ../bin/mycc
LMLC = lmlc$(EXE)
UNLIT = unlit$(EXE)
HBC = hbc$(EXE)
LMLVERSION = ../main/version.m

all:	$(LMLC) $(UNLIT)

$(LMLC):	lmlc.c version.c
	$(CC) $(RCFLAGS) -s $(DQDSTLIB) lmlc.c version.c -o $(LMLC)

$(UNLIT):	unlit.c
	$(CC) $(RCFLAGS) -s unlit.c -o $(UNLIT)

version.c: $(LMLVERSION)
	echo 'char *' > version.c
	sed -n '/version =/s/ "@.*/";/p' $(LMLVERSION) >> version.c
	echo 'char *versiondate =' >> version.c
	sed -n '/DATE/s/--.*/;/p' $(LMLVERSION) >> version.c

install:	all
	-rm -f $(DSTBIN)/$(LMLC) $(DSTBIN)/$(HBC)
	cp $(LMLC) $(DSTBIN)
	-ln $(DSTBIN)/$(LMLC) $(DSTBIN)/$(HBC)
	-rm -f $(DST)/bin/$(UNLIT)
	cp $(UNLIT) $(DST)/bin

clean:
	-rm -f $(LMLC) $(UNLIT)

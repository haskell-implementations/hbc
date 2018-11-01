LMLMK_OBJS = lmlmk.o getdepend.o getmodtime.o makemod.o makeexe.o order.o filenames.o utils.o argv.o env.o cmds.o tsort.o graph.o unlit.o isalpha.o isalnum.o fscache.o table.o tree234.o
OLDER_OBJS = older.o getmodtime.o fscache.o table.o tree234.o utils.o

# Change these if you want.  If you change DST then the scripts lmlmake and hbcmake need to be changed as well.
DSTBIN = /usr/local/bin
DST = /usr/local/lib/lmlc
DSTMAN = /usr/local/man/man1

LDFLAGS = -static

#
DSTLIB = $(DST)/bin

BINS = older$(EXE) lmlmk$(EXE)
SCRIPTS = lmlmake hbcmake
MANS = lmlmake.1 hbcmake.1

LMLC = lmlc
LMLFLAGS=-fzap-redex -fno-strict-call
.SUFFIXES : .m .t

.m.o:
	$(LMLC) -c $(LMLFLAGS) $<

.o.t:
	@echo ''

all: $(BINS)

lmlmk$(EXE) : $(LMLMK_OBJS)
	$(LMLC) $(LMLFLAGS) $(LDFLAGS) $(LMLMK_OBJS) -o lmlmk$(EXE)

older$(EXE) : $(OLDER_OBJS)
	$(LMLC) $(LMLFLAGS) $(LDFLAGS) $(OLDER_OBJS) -o older$(EXE)

clean:
	-rm -f *.o $(BINS)

install: all
	strip $(BINS)
	cp $(BINS) $(DSTLIB)
	cp $(SCRIPTS) $(DSTBIN)

installman:
	cp $(MANS) $(DSTMAN)

tar:
	tar cf lmlmake.tar *.m  *.h HISTORY Makefile Summary TODO hbcmake hbcmakehbc hbcmake.1 lmlmake lmlmake.1

# automatically generated dependencies follows

utils.o: utils.m
isalnum.o: isalnum.m
isalpha.o: isalpha.m
unlit.o: unlit.m
graph.o: graph.m
tsort.o: tsort.m
argv.o: argv.m
tree234.o: tree234.m utils.t
order.o: order.m utils.t tsort.t graph.t
table.o: table.m tree234.t utils.t
fscache.o: fscache.m utils.t table.t
env.o: env.m fs.h fscache.t utils.t unlit.t isalpha.t isalnum.t
getmodtime.o: getmodtime.m fs.h fscache.t
filenames.o: filenames.m env.t
older.o: older.m getmodtime.t
cmds.o: cmds.m env.t argv.t filenames.t utils.t
getdepend.o: getdepend.m fs.h fscache.t filenames.t utils.t env.t
makeexe.o: makeexe.m filenames.t cmds.t
makemod.o: makemod.m getmodtime.t cmds.t filenames.t utils.t
lmlmk.o: lmlmk.m getdepend.t getmodtime.t makemod.t makeexe.t order.t filenames.t utils.t argv.t

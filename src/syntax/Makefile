DST = /usr/local/lib/lmlc
YFLAGS = -dv
CFLAGS = -DYYMAXDEPTH=2000 -g ${SYSTEM}
UGEN = ./ugen
CC = sh ../bin/mycc
YACC = sh ../bin/myyacc

LMLP = lmlp$(EXE)
CURRYP = curryp$(EXE)

LMLOBJS = lmllex.o lml.tab.o
CURRYOBJS = currylex.o curry.tab.o clexeme.o unicode.o
AOBJS = id.o atype.o ttype.o stmt.o tree.o expid.o import.o \
	brack.o sym.o assoc.o kind.o \
	pair.o list.o bind.o pdef.o impid.o finfo.o util.o icmd.o qual.o print.o \
	main.o yyerror.o ctype.o suffix.o readline.o

BOBJS = id.o atype.o ttype.o tree.o expid.o import.o \
	brack.o sym.o assoc.o \
	list.o bind.o pdef.o impid.o finfo.o util.o icmd.o print.o qual.o \
	lextest.o yyerror.o ctype.o

all:	ugen $(LMLP) $(CURRYP)

$(LMLP):	${LMLOBJS} $(AOBJS)
	$(CC) $(STATICFLAG) $(CFLAGS) ${LMLOBJS} $(AOBJS) -o $(LMLP)

$(CURRYP):	${AOBJS} $(CURRYOBJS)
	$(CC) $(STATICFLAG) $(CFLAGS) ${AOBJS} $(CURRYOBJS) -o $(CURRYP)

ugen:
	cd ugendir; make EXE=$(EXE)
	cp ugendir/ugen$(EXE) .

main.o 		: main.c include.h
util.o		: util.c include.h curry.h
currylex.o	: currylex.c curry.h
id.o		: id.c id.h
stmt.c stmt.h	: stmt.ugn
		${UGEN} stmt.ugn
stmt.o		: stmt.c include.h
ttype.c ttype.h	: ttype.ugn
		${UGEN} ttype.ugn
ttype.o		: ttype.c include.h
kind.c kind.h	: kind.ugn
		${UGEN} kind.ugn
kind.o		: kind.c include.h
impid.c impid.h	: impid.ugn
		${UGEN} impid.ugn
impid.o		: impid.c include.h
finfo.c finfo.h	: finfo.ugn
		${UGEN} finfo.ugn
finfo.o		: finfo.c include.h
pdef.c pdef.h	: pdef.ugn
		${UGEN} pdef.ugn
pdef.o		: pdef.c include.h
sym.c sym.h	: sym.ugn
		${UGEN} sym.ugn
sym.o		: sym.c include.h
brack.c brack.h	: brack.ugn
		${UGEN} brack.ugn
brack.o		: brack.c include.h
assoc.c assoc.h : assoc.ugn
		${UGEN} assoc.ugn
assoc.o 	: assoc.c include.h
bind.c bind.h	: bind.ugn
		${UGEN} bind.ugn
bind.o		: bind.c include.h
atype.c atype.h	: atype.ugn
		${UGEN} atype.ugn
atype.o		: atype.c include.h
tree.c tree.h	: tree.ugn 
		${UGEN} tree.ugn
tree.o		: tree.c include.h
list.c list.h	: list.ugn 
		${UGEN} list.ugn
list.o		: list.c include.h
pair.c pair.h	: pair.ugn 
		${UGEN} pair.ugn
pair.o		: pair.c include.h
icmd.c icmd.h	: icmd.ugn 
		${UGEN} icmd.ugn
icmd.o		: icmd.c include.h
qual.c qual.h	: qual.ugn 
		${UGEN} qual.ugn
qual.o		: qual.c include.h
import.c import.h	: import.ugn
		${UGEN} import.ugn
import.o		: import.c include.h
expid.c expid.h	: expid.ugn
		${UGEN} expid.ugn
expid.o		: expid.c include.h
# 
clexeme.o	: clexeme.c
clexeme.c	: curry.h clex.awk
		awk -f clex.awk curry.h > clexeme.c
#
curry.h curry.tab.c   : curry.y
		@echo expect 83 s/r and 0 r/r conflicts
		$(YACC) ${YFLAGS} curry.y
		mv y.tab.h curry.h
		mv y.tab.c curry.tab.c
#
lml.h lml.tab.c	: lml.y
		@echo expect 20 s/r and 5 r/r conflicts
		$(YACC) ${YFLAGS} lml.y
		mv y.tab.h lml.h
		mv y.tab.c lml.tab.c
lmllex.o	: lmllex.c include.h lml.h ctype.h
yyerror.o	: yyerror.c
include.h	: ttype.h id.h list.h tree.h atype.h bind.h pdef.h impid.h finfo.h icmd.h brack.h sym.h assoc.h stmt.h kind.h
parseimp	: parseimp.c
		$(CC) $(CFLAGS) -o parseimp parseimp.c
clean		:
		-rm -f *.o $(LMLP) $(CURRYP) parseimp y.output *.tab.c ugen$(EXE)
		-rm -f lml.h lml.tab.c curry.h curry.tab.c
		-cd ugendir; make clean

print		:
		@pr -l${PS} defs.h main.c id.c id.h \
		            yyerror.c lml.y tree.ugn lml.lex.l Makefile
install		: $(LMLP) $(CURRYP)
		-rm -f $(DST)/bin/$(LMLP) $(DST)/bin/$(CURRYP)
		cp $(LMLP) $(CURRYP) $(DST)/bin
myinstall	: ugen $(LMLP) $(CURRYP)


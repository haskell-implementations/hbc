OBJS =  lex.o parse.o main.o flags.o
MSRC = lex.m parse.m main.m flags.m

OOBJS = ../mcode/mprint.o ../mcode/mcodedef_t.o ../mcode/mutil1.o ../misc/qstring.o ../misc/util.o ../misc/oset.o
MYBIN = ../bin
LMLC = lmlc
LMLFLAGS = -fno-zap-redex
STRIPFLAG= -s
.SUFFIXES :
.SUFFIXES : .m .o .t

.m.o :
	${LMLC} -c ${LMLFLAGS} $<

masm:	${OBJS} ${OOBJS} ../${MTARGET}/*.o
	${LMLC} $(STATICFLAG) ${STRIPFLAG} ${LMLFLAGS} ${OBJS} ${OOBJS} ../${MTARGET}/*.o -o masm$(EXE)

clean:
	-rm -f *.o masm$(EXE)

depend:
	sh ../bin/makedepend ${MSRC}

myinstall: masm
	cp masm$(EXE) ${MYBIN}/masm.${MTARGET}$(EXE)

# DO NOT DELETE THIS LINE

parse.o:  ../mcode/mcodedef_t.t
main.o:  lex.t parse.t ../mcode/machine.tt
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY

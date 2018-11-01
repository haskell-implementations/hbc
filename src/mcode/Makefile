OBJS =  mstrid.o Wuse.o mcodedef_t.o movetext1.o mtype.o mprint.o mvectable.o mstrlbl.o mregs.o mutil1.o mutil.o malloc.o mconst.o mupdate.o mconstrret.o msplit.o mret.o mpush.o mpop.o mmove.o mjglobal.o mjfun.o mmemcheck.o meval.o mconstrupd.o mconstr.o mcbasic.o mget.o mcase.o mbconstr.o mbasicop.o mjmethod.o mbigop.o mlabel.o mjmp.o mfunstart.o mmkapl.o mcode.o

MSRC =	mcodedef_t.m Wuse.m \
	malloc.m mbasicop.m mbconstr.m \
	mcase.m mcbasic.m \
	mconstr.m mconstrupd.m mmkapl.m \
	meval.m mfunstart.m mget.m mjfun.m \
	mjglobal.m mjmp.m mlabel.m mmemcheck.m mmove.m \
	mpop.m mstrid.m mprint.m \
	mpush.m mregs.m mret.m msplit.m \
	mstrlbl.m mtype.m mbigop.m \
	mconstrret.m mupdate.m mutil.m \
	mcode.m mutil1.m movetext1.m mjmethod.m mvectable.m mconst.m

LMLC = lmlc
.SUFFIXES :
.SUFFIXES : .m .o .t
.PHONY : all

.m.o:
	${LMLC} -c ${LMLFLAGS} $<

all : 	${OBJS}

oosave:
	mv *.o saveoo
unosave:
	mv saveoo/*.o .
unsave:
	mv saveo/*.o .
osave:
	mv *.o saveo

countlines:
	wc $(MSRC)

clean:
	-rm -f *.o
	-rm -f saveo*/*

depend:
	sh ../bin/makedepend ${MSRC}

# DO NOT DELETE THIS LINE

malloc.o: ../misc/misc.t mcodedef_t.t ../Gcode/Gcodedef_t.t ../Gcode/Gprint.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t handmade.mcode.t mvectable.t mprint.t
mbasicop.o: mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t handmade.mcode.t machine.tt
mbconstr.o: mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t handmade.mcode.t mvectable.t machine.tt
mcase.o: mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t handmade.mcode.t mtype.t mget.t machine.tt ../misc/flags.t ../runtime/tagtable.h
mcbasic.o: mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t handmade.mcode.t mvectable.t limit.h machine.tt
mconstr.o: ../misc/flags.t mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t mvectable.t handmade.mcode.t limit.h
mconstrupd.o: mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t handmade.mcode.t mvectable.t ../misc/flags.t machine.tt
mmkapl.o: ../expr/id.t mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t handmade.mcode.t mconstr.t
meval.o: mcodedef_t.t ../Gcode/Gcodedef_t.t ../misc/flags.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t mprint.t handmade.mcode.t mmemcheck.t machine.tt
mfunstart.o: ../expr/id.t mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t handmade.mcode.t ../misc/setofid.t ../misc/flags.t mmemcheck.t limit.h mvectable.t machine.tt
mget.o: mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t handmade.mcode.t machine.tt
mjfun.o: ../misc/flags.t mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t handmade.mcode.t machine.tt
mjglobal.o: ../expr/id.t ../misc/flags.t mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t handmade.mcode.t ../funnos.h
mjmp.o: mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t handmade.mcode.t mmemcheck.t
mlabel.o: mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t handmade.mcode.t mmemcheck.t
mmemcheck.o: ../misc/flags.t mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t mprint.t handmade.mcode.t machine.tt limit.h
mmove.o: mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t handmade.mcode.t mprint.t
mpop.o: mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t mprint.t handmade.mcode.t
mstrid.o: ../misc/setofid.t ../expr/id_t.t ../expr/id.t
mprint.o: mcodedef_t.t Wuse.t
mpush.o: mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t mprint.t mvectable.t handmade.mcode.t
mregs.o: mcodedef_t.t machine.tt
mret.o: ../misc/flags.t mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t handmade.mcode.t machine.tt
msplit.o: mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t mprint.t handmade.mcode.t
mstrlbl.o: ../Gcode/Gcodedef_t.t
mtype.o: ../Gcode/Gcodedef_t.t limit.h
mbigop.o: mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t handmade.mcode.t mmemcheck.t machine.tt
mconstrret.o: mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t handmade.mcode.t mvectable.t ../misc/flags.t machine.tt
mupdate.o: mcodedef_t.t ../Gcode/Gcodedef_t.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t handmade.mcode.t machine.tt ../misc/flags.t
mutil.o: ../misc/flags.t ../Gcode/Gcodedef_t.t machine.tt mcodedef_t.t Wuse.t mregs.t mvectable.t
mcode.o: ../expr/constr_t.t ../expr/id.t ../ExprE/Expr_t.t ../Gcode/Gcodedef_t.t ../Gcode/Gprint.t ../misc/setofid.t ../misc/flags.t ../misc/misc.t ../transform/hexpr.t mcodedef_t.t movetext1.t mutil.t mutil1.t mstrid.t mpush.t meval.t mbigop.t mbasicop.t mpop.t mconstr.t mconst.t mtype.t mmkapl.t mconstrret.t mcbasic.t msplit.t mupdate.t mconstrupd.t mbconstr.t mlabel.t mjmp.t mjfun.t malloc.t mmove.t mcase.t mget.t mfunstart.t mret.t mjglobal.t mjmethod.t mmemcheck.t mvectable.t
mutil1.o: ../Gcode/Gcodedef_t.t mcodedef_t.t
movetext1.o: mcodedef_t.t
mjmethod.o: mcodedef_t.t ../transform/hexpr.t ../Gcode/Gcodedef_t.t ../Gcode/Gprint.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t mprint.t mmemcheck.t handmade.mcode.t machine.tt
mvectable.o: mcodedef_t.t ../misc/flags.t ../main/files.t magic.h
mconst.o: ../expr/id.t ../expr/constr_t.t ../misc/flags.t mcodedef_t.t ../Gcode/Gcodedef_t.t ../Gcode/Gprint.t Wuse.t mutil.t mutil1.t mregs.t mstrlbl.t mstrid.t mvectable.t handmade.mcode.t limit.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY

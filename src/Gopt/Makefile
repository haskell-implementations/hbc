OBJS =  goptgm.o ppeepopt.o peepopt.o jfunutil.o jfun2.o jfun1.o stackstub.o jfun.o jumpopt.o gopt.o
MSRC = jfunutil.m jfun1.m jfun2.m jfun.m jumpopt.m peepopt.m gopt.m goptgm.m ppeepopt.m stackstub.m

LMLC = lmlc
.SUFFIXES :
.SUFFIXES : .m .o .t
.PHONY: all

.m.o :
	${LMLC} -c ${LMLFLAGS} $<

all:	${OBJS}

clean:
	-rm -f *.o
	-rm -f saveo*/*

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

depend:
	sh ../bin/makedepend ${MSRC}

# DO NOT DELETE THIS LINE

jfunutil.o: ../expr/id.t ../Gcode/Gcodedef_t.t
jfun1.o: ../Gcode/Gcodedef_t.t jfunutil.t
jfun2.o: jfunutil.t
jfun.o: ../Gcode/Gcodedef_t.t ../misc/misc.t jfunutil.t jfun1.t jfun2.t
jumpopt.o: ../Gcode/Gcodedef_t.t jfun.t
peepopt.o: ../Gcode/Gcodedef_t.t
gopt.o: ../Gcode/Gcodedef_t.t peepopt.t ppeepopt.t jumpopt.t stackstub.t
goptgm.o: ../Gcode/Gcodedef_t.t
ppeepopt.o: ../Gcode/Gcodedef_t.t
stackstub.o: ../Gcode/Gcodedef_t.t ../misc/flags.t ../Gcode/Gprint.t jfun1.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY

OBJS =  Gmode_t.o Gcodedef_t.o Gprint.o Gid.o Gseq.o Genv.o Gfreeset.o Gconst.o Gtail.o Gcase.o Gcode.o Gutil.o Stingycode.o Gmain.o
MSRC = Gcodedef_t.m Gmode_t.m \
	Gfreeset.m Genv.m Gseq.m Gutil.m Gcase.m Gtail.m \
	Gcode.m Gprint.m Gmain.m Stingycode.m Gid.m Gconst.m

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
osave:
	mv *.o saveo
unsave:
	mv saveo/*.o .

countlines:
	wc $(MSRC)

depend:
	sh ../bin/makedepend ${MSRC}

# DO NOT DELETE THIS LINE

Gfreeset.o: ../misc/setofid.t ../ExprE/Expr_t.t ../ExprE/Eprint.t
Genv.o: ../expr/id.t ../misc/util.t
Gseq.o: Gcodedef_t.t
Gutil.o: ../expr/einfo_t.t ../expr/ttype_t.t ../expr/id_t.t ../expr/id.t ../expr/constr_t.t ../ExprE/Expr_t.t ../misc/flags.t ../funnos.h Gseq.t Genv.t Gcodedef_t.t Gmode_t.t Gcode.t
Gcase.o: ../expr/id_t.t ../expr/id.t ../expr/constr_t.t ../expr/constrfun.t ../expr/einfo_t.t ../ExprE/Expr_t.t ../misc/flags.t Genv.t Gseq.t Gutil.t Gmode_t.t Gcodedef_t.t Gcode.t
Gtail.o: ../expr/einfo_t.t ../expr/id_t.t ../ExprE/Expr_t.t ../ExprE/Eprint.t ../misc/setofid.t Genv.t Gutil.t Gseq.t Gfreeset.t Gmode_t.t Gcodedef_t.t Gcode.t
Gcode.o: ../expr/id_t.t ../expr/id.t ../expr/ttype.t ../expr/einfo_t.t ../expr/constr_t.t ../expr/constrfun.t ../ExprE/Eprint.t ../ExprE/Expr_t.t ../ExprE/Eutil.t ../misc/flags.t ../misc/misc.t ../funnos.h Gcodedef_t.t Gmode_t.t Gseq.t Genv.t Gcase.t Gconst.t Gtail.t Gutil.t Gid.t Gprint.t
Gprint.o: ../misc/pri.t ../misc/text_t.t ../misc/Tflat.t ../expr/id_t.t ../expr/id.t ../expr/constr_t.t Gcodedef_t.t
Gmain.o: ../expr/einfo_t.t ../expr/id_t.t ../expr/id.t ../ExprE/Expr_t.t ../misc/flags.t Genv.t Gseq.t Gmode_t.t Gcodedef_t.t Gcode.t Stingycode.t ../funnos.h
Stingycode.o: ../expr/id_t.t ../expr/id.t ../expr/einfo_t.t ../expr/constr_t.t ../expr/constrfun.t ../ExprE/Eprint.t ../ExprE/Expr_t.t ../ExprE/Eutil.t ../misc/flags.t ../funnos.h Gcodedef_t.t Gmode_t.t Gseq.t Genv.t Gcase.t Gtail.t Gutil.t Gcode.t
Gid.o: ../expr/types_t.t ../expr/id_t.t ../expr/ttype_t.t ../expr/booltree.t ../expr/ttype.t ../expr/einfo_t.t ../expr/id.t ../funnos.h
Gconst.o: ../expr/id.t ../expr/einfo_t.t ../misc/flags.t ../expr/constrfun.t ../ExprE/Expr_t.t Gcodedef_t.t Genv.t Gutil.t Gid.t Gprint.t ../funnos.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY

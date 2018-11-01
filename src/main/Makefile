OBJS =  version.o files.o topexprs.o main.o
MSRC = main.m files.m version.m topexprs.m

LMLC = lmlc
.SUFFIXES :
.SUFFIXES : .m .o .t
.PHONY : all

.m.o :
	${LMLC} -c ${LMLFLAGS} $<

all:	${OBJS}

oosave:
	mv *.o saveoo
unosave:
	mv saveoo/*.o .
osave:
	mv *.o saveo
unsave:
	mv saveo/*.o .
clean:
	-rm -f *.o
	-rm -f saveo*/*

countlines:
	wc $(MSRC)

depend:
	sh ../bin/makedepend $(MSRC)

# DO NOT DELETE THIS LINE

main.o: ../misc/flags.t ../flic/flic.t ../flic/fconstr.t ../bwm/bwm.t ../expr/pprint.t ../expr/error.t ../expr/types_t.t ../expr/id.t ../expr/ttype.t ../rename/renameutil.t ../rename/renenv.t ../transform/match.t ../transform/constr.t ../transform/lettrans.t ../transform/remsign.t ../transform/remclass.t ../transform/remlazy.t ../ExprE/Ecnv.t ../ExprE/Eprint.t ../ExprE/apconv.t ../ExprE/globalize.t ../ExprE/eqtrans.t ../ExprE/classtrans.t ../ExprE/addrestr.t ../ExprE/prexport.t ../ExprE/hprexport.t ../ExprE/unrec.t ../ExprE/predef.t ../ExprE/forcearity.t ../ExprE/addspec.t ../ExprE/remunused.t ../type/tchk.t ../type/prefix.t ../etype/cunrec.t ../etype/tchk.t ../etype/substear.t ../etype/xsubst.t ../llift/addclose.t ../llift/llift.t ../llift/Bconv.t ../llift/addarity.t ../llift/chkind.t ../Gcode/Gcodedef_t.t ../Gcode/Gmain.t ../Gcode/Gprint.t ../Gopt/gopt.t ../Gopt/goptgm.t ../mcode/mcode.t ../mcode/mprint.t ../mopt/mopt.t ../strict/strict.t ../strict/mkstrict.t ../strict/strictcon.t ../strict/calcstrict.t ../simpl/simpl.t ../simpl/flow.t ../mcode/machine.tt ../zf/remzf.t ../misc/ioc.t ../ilml/imain.t topexprs.t files.t version.t
files.o: ../misc/flags.t
topexprs.o: ../misc/flags.t ../flic/flic.t ../expr/read.t ../zf/lmlzf.t ../curry/curry0.t ../rename/rename.t ../rename/renenv.t ../ilml/icomp.t files.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY

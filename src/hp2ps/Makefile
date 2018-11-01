OBJS= 	\
	AuxFile.o	\
	Axes.o		\
	AreaBelow.o	\
	Curves.o 	\
	Deviation.o	\
	Dimensions.o	\
	Error.o 	\
	HpFile.o	\
	Key.o		\
	Main.o 		\
	Marks.o		\
	TopTwenty.o	\
	TraceElement.o	\
	PsFile.o 	\
	Reorder.o	\
	Scale.o		\
	Shade.o 	\
	Utilities.o

# Please set MATHLIB and BIN appropriately. I don't need MATHLIB on my machine,
# but you may.

MATHLIB = -lm

DSTBIN=/usr/local/bin

CC= sh ../bin/mycc
CCFLAGS=
LDFLAGS= ${STATICFLAG}

TARGET=hp2ps$(EXE)

${TARGET}: ${OBJS}
	${CC} ${LDFLAGS} ${CCFLAGS} ${OBJS} ${MATHLIB} -o ${TARGET}

install: ${TARGET}
	cp ${TARGET} ${DSTBIN}

clean:
	-rm -f core *.o ${TARGET}

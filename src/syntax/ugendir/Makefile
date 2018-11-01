CC = sh ../../bin/mycc
YACC = sh ../../bin/myyacc
LEX = sh ../../bin/mylex
UGEN = ugen$(EXE)

$(UGEN) 	: main.o gen.o lex.yy.o y.tab.o id.o tree.o yyerror.o
	$(CC) -s main.o gen.o lex.yy.o y.tab.o id.o tree.o yyerror.o -o $(UGEN)

main.o 		: main.c tree.h
		$(CC) -c main.c
gen.o		: gen.c tree.h
		$(CC) -c gen.c
lex.yy.c	: lex.l y.tab.h
		$(LEX) lex.l
lex.yy.o 	: lex.yy.c y.tab.h
		$(CC) -c lex.yy.c
y.tab.o		: y.tab.c tree.h
		$(CC) -c y.tab.c 
id.o		: id.c id.h
		$(CC) -c id.c
#tree.c tree.h	: tree.ugn
#		ugen tree.ugn
tree.o		: tree.c tree.h id.h
		$(CC) -c tree.c
y.tab.h		: syntax.y
		$(YACC) -d syntax.y
y.tab.c		: syntax.y tree.h
		$(YACC) -d syntax.y
yyerror.o	: yyerror.c
		$(CC) -c yyerror.c

install		:
		cp $(UGEN) ..

clean		:
		rm -f $(UGEN) *.o lex.yy.c y.tab.c y.tab.h y.output


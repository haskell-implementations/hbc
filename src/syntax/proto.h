#if defined(__ANSI__) || defined(__GNUC__)
#define PROTO(x) x
#else
#define PROTO(x) ()
#endif

/*void error PROTO((char *));*/
void error();
/*void errmsg PROTO((char *, char *));*/
void errmsg();
void yyerror PROTO((char *));
void syntaxerror PROTO((void));
void obsolete PROTO((char *));
int popfile PROTO((void));
void secprompt PROTO((void));
int switchtoid PROTO((char *));

void makeinfix PROTO((char *, int));
void makefixop PROTO((char *, int, int));
int nfixes PROTO((void));
int fixtype PROTO((int));
char *fixop PROTO((int));

id installid PROTO((char *));

void init PROTO((void));
void initlex PROTO((void));
void pushextra PROTO((int));
int yyparse PROTO((void));

int yylex PROTO((void));

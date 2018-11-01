#include "include.h"
#include "suffix.h"
#include "lml.h"
#include "ctype.h"
#include <sys/types.h>
#include <sys/stat.h>

#include "proto.h"

#define MAXINFIX 200
#define MAXISTR 5000

#define COMM 10000
#define PLCOMM 10001

char *cppname;

extern id installid();
extern int interactive;

extern char loadname[];

extern int debug;
int lasttoken;
int yylineno = 1;
extern int zyzflag, zyzid;
char *filename;
FILE *yyin;
FILE *oldfiles[100];
static unsigned char yybuf[5120];
int oldindex = 0;
static int pushtoken = -1;
static int forcedeof = 0;
static unsigned char *yyp;
static char tmpname[200] = "";
static char incdir[1024];
unsigned char yytext[512];
static int saved = 0;
static unsigned char savechar[1024];
#define LASTMAX 10
static int curchar='\n', lastvec[LASTMAX];
static int lastind = 0;
#define lastchar lastvec[lastind % LASTMAX]
static list loadlist;

static struct yytable {
    char *name;
    short token;
} resvtable[] = {
    { "let",	LET },
#ifdef OBSOLETE
    { "letrec",	LETREC },
#endif /* OBSOLETE */
    { "where",	WHERE },
#ifdef OBSOLETE
    { "whererec",	WHEREREC },
#endif /* OBSOLETE */
    { "in",	IN },
    { "rec",	REC },
    { "type",	TYPE },
    { "conctype",	CONCTYPE },
    { "leftassoc",  LEFTASSOC },
    { "rightassoc", RIGHTASSOC },
    { "nonassoc",   NONASSOC },
    { "and",	AND },
    { "if",	IF },
    { "then",	THEN },
    { "else",	ELSE },
    { "module",	MODULE },
    { "end",	END },
    { "import",	IMPORT },
    { "export",	EXPORT },
    { "case",	CASE },
    { "as",	AS },
    { "local",	LOCAL },
    { "o",	FCOMP },
    { "infix",	MINFIX },
    { "infixl",	MINFIX },
    { "infixr",	MINFIXR },
    { "prefix",	MPREFIX },
    { "postfix",	MPOSTFIX },
    { "nonfix",	MNONFIX },

    { "load",	LOAD },
    { "source",	SOURCE },
    { "quit",	THEEND },
    { "whatis",	WHATIS },
    { "help",	HELP },
    { "show_",	SHOW_ },
    { 0,0 },
    { 0,0 }
};
struct yytable annottable[] = {
    { "LINE",	A_LINE },
    { "INLINE",	A_INLINE },
    { "EXPAND",	A_EXPAND },
    { "ARITY",	A_ARITY },
    { "FRAMESIZE", A_FRAMESIZE },
    { "STRICTNESS",A_STRICTNESS },
    { "STRICT",	 A_STRICT },
    { "SHARED",	 A_SHARED },
    { "UNSHARED",	 A_UNSHARED },
    { "SPARK",	 A_SPARK },
    { "ENTRY",	 A_ENTRY },
    { "NOEVAL",    A_NOEVAL },
    { "OVERLOAD",  A_OVERLOAD },
    { "FLAT",      A_FLAT },
    { "DERIVED",   A_DERIVED },
    { "FROMMODULE",A_FROMMODULE },
    { "SPECIALIZE",A_SPECIALIZE },
    { "EVALED",    A_EVALED },
    { "VECTORDEF", A_VECTORDEF },
    { "VECREG2",   A_VECREG2 },
    { "LIMITOK",   A_LIMITOK },
    { "METCALL",   A_METCALL },
    { "NOTCHK",	 A_NOTCHK },
    { 0,0, },
    { 0,0 }
};

#define COMM 10000
static struct infix {
    char *iname;	/* first char is ignored */
    short ilen;
    short itoken;
} infixtab[MAXINFIX] = {
{ "?{#",  2,      NANNOT },
{ "?{:",  2,      ANNOT },
{ "?#}",  2,      ENDANNOT },
{ "?--",	2,	COMM },
{ "?/*",	2,	PLCOMM },
{ "?||",	2,	GUARD },
{ "?;;",	2,	SUCH },
{ "?<-",	2,	LARROW },
{ "?..",	2,	DOTDOT },
{ "_~=",	2,	NE },
{ "_<=",	2,	LE },
{ "P->",	2,	TFUN },
{ "_??",	2,	LINDEX },
{ "_+.",	2,	FPLUS },
{ "_-.",	2,	FMINUS },
{ "_*.",	2,	FTIMES },
{ "_/.",	2,	FDIV },
{ "_+#",	2,	IPLUS },
{ "_-#",	2,	IMINUS },
{ "_*#",	2,	ITIMES },
{ "_/#",	2,	IDIV },
{ "_%#",	2,	IMOD },
{ "?::",  2,      DCOLON },
{ "?:=",  2,      ASSIGN },

{ "?\\",	1,	LAMBDA },
{ "_.",	1,	DOT },
{ "?;",	1,	SEMI },
{ "P,",	1,	COMMA },
{ "_=",	1,	EQ },
{ "_<",	1,	LT },
{ "_+",	1,	PLUS },
{ "_-",	1,	MINUS },
{ "_*",	1,	TIMES },
{ "_/",	1,	DIV },
{ "_%",	1,	MOD },
{ "_~",	1,	NOT },
{ "_&",	1,	ANDOP },
{ "_|",	1,	OROP },
{ "?:",	1,	COLON },
{ "_@",	1,	CONC },
{ "?]",	1,	RBRACK },
{ "?{",	1,	LBRACE },
{ "?}",	1,	RBRACE },
{ "?!",	1,	EXCL },
{ "_?",	1,	INDEX },
{ "_^",	1,	RAISE },

{ "?",	0,	0 },

{ 0,	0,	0 },
};

static int ninfix;
static char infixstr[MAXISTR];
static char *infixp = infixstr;
#define mlinf (infixtab[0].ilen)	/* longest length */
static int concflag = 0;   		/* for conctypes  */
					/* >0 if inside a concquote */
static int antiquoteflag = 0;		/* for conctypes */
					/* >0 if inside an antiquote  */
static int parcount = 0;		/* for conctypes  */
					/* =n if n left '(' has been seen  */
					/* inside an antiquotation  */
static int parcountstack[100];
static int nonterminalflag = 0;		/* for conctypes  */
					/* =1 if inside a nonterminal  */

int
nfixes()
{
    return ninfix;
}

char *
fixop(n)
int n;
{
    return infixtab[n].iname;
}

int
fixtype(n)
int n;
{
    switch(infixtab[n].itoken) {
    case INFIXL : return 1;
    case INFIXR : return 2;
    case PREFIX : return 3;
    case NONFIX : return 5;
    case POSTFIX : return 4;
    default : return -1;
    }
}

static void
processbackslash(q, p)
register char *p, *q;
{
    register int c;

    while(*p) {
	if ((c = *p++) == '\\') {
	    switch(c = *p++) {
	    case 'n': c = '\n'; break;
	    case 't': c = '\t'; break;
	    case 'r': c = '\r'; break;
	    case 'b': c = '\b'; break;
	    case 'f': c = '\f'; break;
	    case 'e': c = '\033'; break;
	    case '0': case '1': case '2': case '3':
	    case '4': case '5': case '6': case '7':
		c -= '0';
		if ('0' <= *p && *p <= '7')
		    c = c*8 + *p++-'0';
		if ('0' <= *p && *p <= '7')
		    c = c*8 + *p++-'0';
		break;
	    }
	}
	*q++ = c;
    }
    *q = 0;
}


void
makeinfix(ss, token)
char *ss;
int token;
{
    register int i, l;
    char s[1000], *p;

    p = s;
    if (!zyzflag)
	*p++ = '_';
    strcpy(p, ss);
    l = strlen(s);
    if (ninfix >= MAXINFIX || infixp+l+1 >= &infixstr[MAXISTR]) {
	error("Too many infixes.");
	return;
    }
    for(i = ninfix; i > 0; i--) {
	if (infixtab[i-1].ilen >= l)
	    break;
	infixtab[i] = infixtab[i-1];
    }
    infixtab[i].iname = infixp;
    strcpy(infixp, s);
    infixp += l+1;
    infixtab[i].itoken = token;
    infixtab[i].ilen = l-1;
    ninfix++;
}

void
yyinit()
{
    yyin = stdin;
    cppname = 
#if defined(__NetBSD__) || defined(__FreeBSD__)
      "/usr/libexec/cpp -traditional -$"
#elif defined(linux)
      "/lib/cpp -traditional -$"
#elif defined(SOLARIS) || defined(_AIX)
      "/usr/ccs/lib/cpp"
#else
      "/lib/cpp"
#endif
      ;
    for(ninfix = 0; infixtab[ninfix].iname; ninfix++)
	;
    saved = 0;
    yylineno = 1;
}

static struct yytable *
lookup(name, tab)
char *name;
struct yytable *tab;
{
    register struct yytable *p;

    for(p = tab; p->name; p++)
	if (strcmp(name, p->name) == 0)
	    return p;
    return 0;
}

static int
letter(c)
register c;
{
    return isalnum(c) || c == '_' || c == '\'' || (zyzflag && c == '#');
}

static int
yygetc()
{
    int c;
    static int seeneof = 0;

    lastvec[++lastind % LASTMAX] = curchar;
    if (saved > 0) {
	c = savechar[--saved];
    } else {
	if (seeneof)
	    c = EOF;
	else
	    c = getachar(interactive,yyin);
    }
    if (c == '\n')
	yylineno++;
    if (c != EOF) {
	*yyp++ = c;
    } else if (oldindex == 0)
	seeneof++;
    curchar = c;
    return c;
}

static void
yyunget(c)
int c;
{
    if (c != EOF) {
	savechar[saved++] = c;
	if (c == '\n')
	    yylineno--;
	yyp--;
	curchar = lastvec[lastind-- % LASTMAX];
    }
}

static int
yypeek()
{
    register int c;

    c = yygetc();
    yyunget(c);
    return c;
}

static int
yypeek2()
{
    register int c, r;

    c = yygetc();
    r = yypeek();
    yyunget(c);
    return r;
}

static int
gobble(c)
register c;
{
    register int d;

    d = yygetc();
    if (d != c) {
	yyunget(d);
	return 0;
    } else {
	return 1;
    }
}

static void
skipdigits()
{
    register int c;

    do {
	c = yygetc();
    } while(isdigit(c));
    yyunget(c);
}

static void
skipto(c1, c2)
int c1, c2;
{
    int c;

    do {
	while((c = yygetc()) != c1 && c != EOF)
	    yyp = yytext;
    } while (yypeek() != c2 && yypeek() != EOF);
    (void)yygetc();
}

static int
yylex1()
{
    register int i, j, c;
    register struct yytable *p;
    int t;
    unsigned char ibuff[100];
    static int inannot = 0;

    if (pushtoken >= 0) {
	t = pushtoken;
	pushtoken = -1;
	return t;
    }
    if (forcedeof)
	return EOF;
    for (;;) {
    again:
	yyp = yytext;
    	/* first try for an infix operator */
	for(i = 0; i < mlinf && yypeek() != '\n' && yypeek() != EOF; i++) {
	    ibuff[i] = yygetc();
	}
	ibuff[i] = 0;
	for(j = 0; j < ninfix; j++) {
	    while (infixtab[j].ilen < i) {
	        yyunget(ibuff[--i]);
		ibuff[i] = 0;
	    }
	    if (i == 0)
	    	break;
	    if (strcmp((char *)ibuff, infixtab[j].iname+1) == 0) {
	    	t = infixtab[j].itoken;
		if (t == COMM) {
		    /* Skip the comment */
		    while ((c = yygetc()) != '\n' && c != EOF)
		        ;
		    yyunget(c);
		    goto again;
		} else if (t == PLCOMM) {
		    skipto('*', '/');
		    goto again;
		} else if (t == ANNOT) {
		    while(!(yygetc() == ':' && yypeek() == '}'))
			;
		    yygetc();
		    yyp[-2] = 0;
		    if (zyzid && yytext[2] == '"') {
			/* special id */
			*strrchr((char *)yytext+3, '"') = 0;
			yylval.uid = installid(yytext+3);
			return ID;
		    } else
			return SYNTAX_ERROR;
		} else if (t == NANNOT) {
		    inannot++;
		    while(isspace(yypeek()))
			yygetc();
		    yyp = yytext;
		    do {
			c = yygetc();
		    } while(c != EOF && letter(c) && c != '#');
		    yyunget(c);
		    *yyp = 0;
		    p = lookup(yytext, annottable);
		    if (!p) {
			/* Not recognised, skip it */
			skipto('#', '}');
			goto again;
		    }
		    if (p->token == A_LINE) {
			char tmpname[1000];
			yyp = yytext;
			while((c = yygetc()) != '\n' && c != EOF)
			    ;
			*yyp = 0;
			if (sscanf((char *)yytext, "%d \"%[^\"]", &yylineno, tmpname) == 2) {
			    filename = installid(tmpname);
			}
			goto again;
		    }
		    return p->token;
		} else if (t == ENDANNOT) {
		    if (!inannot)
			yyerror("Spurious #}");
		    inannot--;
		    return t;
		} else {
		    yylval.uid = infixtab[j].iname;
		    return t;
		}
	    }
	}
	c = yygetc();
	switch (c) {
	case EOF: 
	    if (oldindex) {
		return popfile();
	    } else
		if (interactive) {
		    interactive = 0;
		    return THEEND;
		} else
		    return EOF;
	case '\n': 
	    if (lasttoken != SEMI && interactive && !oldindex)
		secprompt();
	case ' ': 
	case '\t': 
	    break;
#if 0
	case '{':
	    c = yypeek();
	    for(;;) {
		switch(yygetc()) {
		case '}':
		    *--yyp = 0;
		    yylval.uid = installid(yytext+1);
		    return ANNOT;
		case '\\':
		    if (yygetc() == '\n')
			yyp--;
		    break;
		case '\n':
		case EOF:
		    return SYNTAX_ERROR;
		}
	    }
#else
	    /* temporary backwards compat fix, ignore old annotations */
	case '{':
	    while(yygetc() != '}')
		;
	    break;
#endif

	hash:
	case '#':
	    if (lastchar == '\n') {
		char tmpname[1000];
		unsigned char *q;
		while((c = yygetc()) != '\n' && c != EOF)
		    ;
		*yyp = 0;
		q = yytext+1;
		if (strncmp((char *)q, "line", 4) == 0)
		    q += 4;
		if (sscanf((char *)q, "%d \"%[^\"]", &yylineno, tmpname) < 1) {
		    if (sscanf((char *)q, "line %d \"%[^\"]", &yylineno, tmpname) < 1) {
			fprintf(stderr, "Bad # line: %s\n", yytext);
			exit(1);
		    }
		}
		filename = installid(tmpname);
	    } else if (isdigit(yypeek())) {
		while(isdigit(yypeek()))
		    (void)yygetc();
		*yyp = 0;
		goto identi;
	    } else {
		return TPAIR;
	    }
	    break;
	case '\'':
	    switch(c = yygetc()) {
	    case '\\':
		switch(c = yygetc()) {
		case 'n': c = '\n'; break;
		case 't': c = '\t'; break;
		case 'r': c = '\r'; break;
		case 'b': c = '\b'; break;
		case 'f': c = '\f'; break;
		case 'e': c = '\033'; break;
		case '0': case '1': case '2': case '3':
		case '4': case '5': case '6': case '7':
		    c -= '0';
		    if ('0' <= yypeek() && yypeek() <= '7')
			c = c*8 + yygetc()-'0';
		    if ('0' <= yypeek() && yypeek() <= '7')
			c = c*8 + yygetc()-'0';
		    break;
		}
	    default:
		yytext[0] = c;
		yytext[1] = 0;
		yylval.uid = installid(yytext); 
		break;
	    }
	    if (gobble('\''))
		return CHAR;
	    else
		return SYNTAX_ERROR;
            case '(':					 /* for conctypes < */
		if (antiquoteflag && antiquoteflag==concflag && !nonterminalflag)
		   parcount++;
		return LPAR;
	    case ')':
		if (antiquoteflag && antiquoteflag==concflag && !nonterminalflag) {
		    if (--parcount == 0) {
			antiquoteflag--;
			parcount = parcountstack[antiquoteflag];
		    }
		}
		return RPAR;
	    case '>':
		if (nonterminalflag) {
		   nonterminalflag--;
		   return RNONTERM;
		}
		else if (yypeek() == '=') {
                         yygetc();
			 yylval.uid = "_>=";
                         return GE;
                     } else {
			 yylval.uid = "_>";
			 return GT;
		     }
	    case '[':
		switch(yypeek()) {
		    case '|':
			(void)yygetc();
			concflag++;
			switch(yypeek()) {
			  case '|':
			       c = yygetc();
			       switch(yypeek()) {
				 case ']' : 
				      yyunget(c);
				      return LQUOTE;
				 default :
			              return LFORGET;
			       }
			  default:
			       return LQUOTE;
			}
                    default:
			return LBRACK;
                }			                  /* for conctypes > */
	case '"':
	    do {
		c = yygetc();
		if (c == EOF || c == '\n')
		    yyerror("Nonterminated string");
		if (c == '\\')
		    if (yygetc() == EOF)
			yyerror("Bad escape char");
	    } while (c != '\"');
	    *--yyp = 0;
	    processbackslash(yybuf, yytext+1);
	    /* Change all tabs back to \t */
	    { unsigned char *p, *q;
	      for(q = yybuf, p = yytext; *q; p++, q++) {
		  if ((*p = *q) == '\t')
		      *p++ = '\\', *p = 't';
		  else if (*q == '\\')
		      *p++ = '\\', *p = '\\';
	      }
	      *p = 0;
	    }
	    yylval.uid = installid(yytext);
	    return STRING;

	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	    if ((c = yypeek()) == 'x' || c == 'X') {
		/* hex number */
		yygetc();	/* 'x' */
		if (!isxdigit(yypeek()))
		    return SYNTAX_ERROR;
		do {
		    c = yygetc();
		} while(isxdigit(c));
		yyunget(c);
		*yyp = 0;
		sscanf((char *)yytext+2, "%lx", &yylval.uuint);
		return INTCONST;
	    }

	    skipdigits();
	    c = yypeek();
	    if ((c == '.' && isdigit(yypeek2())) || c == 'e' || c == 'E') {
		if (c == '.') {
		    (void)yygetc();
		    skipdigits();
		    c = yypeek();
		}
		if (c == 'e' || c == 'E') {
		    (void)yygetc();
		    c = yypeek();
		    if (c == '-' || c == '+')
			(void)yygetc();
		    skipdigits();
		}
		*yyp = 0;
		yylval.ufloat = atof((char *)yytext);
		return FLOATCONST;
	    } else if (c == 'I' || c == '#') {
		/* A bignum */
		(void)yygetc();
		*yyp = 0;
		yyp[-1] = 'I';
		yylval.uid = installid(yytext);
		return INTEGERCONST;
	    } else {
		*yyp = 0;
		yylval.uuint = atoi((char *)yytext);
		return INTCONST;
	    }
	case '_':
	    if (yypeek() == '#') {
		(void)yygetc();
		goto hash;
	    }
	    /* fall into */
	case '$':
	    if (!letter(yypeek())) {
		*yyp = 0;
		yylval.uid = "_";
		return WILD;
	    }
	default:
	    if (letter(c)) {
		do {
		    c = yygetc();
		} while(c != EOF && letter(c));
		yyunget(c);
		*yyp = 0;
		if ((p = lookup(yytext, resvtable))) {
#if 1
		    if (p->token == LETREC || p->token == WHEREREC)
			obsolete(yytext);
#endif
		    if (!interactive && p->token >= LOAD)
			goto identi;
		    if (p->token == FCOMP)
			yylval.uid = "_o";
		    return p->token;
		} else {
		    char tt[1000];
		identi:
#ifdef OBSOLETE
		    if (strcmp(yytext,"int" )==0 || strcmp(yytext,"bool")==0 ||
			strcmp(yytext,"char")==0 || strcmp(yytext,"list")==0) {
			obsolete(yytext);
			yytext[0] = toupper(yytext[0]);
		    }
#endif
		    sprintf(tt, "%s%s", zyzflag ? "" : "_", yytext);
		    yylval.uid = installid(tt);
		    return ID;
		}
	    } else {
		return SYNTAX_ERROR;
	    }
	}
    }
}

int
conclex()						/* for conctypes */
{
	register int c,c1,c2;

	c = yygetc();
	switch(c) {
	    case EOF:
		return EOF;
	    case ' ':
	    case '\t':
	    case '\n':
	        while(c==' ' || c=='\t' || c=='\n')
		  c = yygetc();
	        yyunget(c);
	        yylval.uterm = ' ';
	        return TERMINAL;
	    case '\\':
	        yylval.uterm = yygetc();
	        return TERMINAL;
	    case '^':
		if (yypeek()=='(') {
		    parcountstack[antiquoteflag] = parcount;
		    parcount = 0;
		    antiquoteflag++;
		    return ANTIQUOTE;
		}
		if (yylex1()==ID) {
	           while(yypeek()==' ')
		        c = yygetc();
		   return ANTIQVAR;
	        }
		yyerror("Expected an ident or ( after antiquote");
		return SYNTAX_ERROR;
	    case '<':
		nonterminalflag++;
		return LNONTERM;
            case '|':
		if (yypeek() == ']'){
		   yygetc();
		   concflag--;
		   return RQUOTE;
		}
		if (yypeek() == '|'){
		   yygetc();
		   if (yypeek() == ']'){
		       yygetc();
		       concflag--;
		       return RFORGET;
		   } else yyunget('|');
		}
		yylval.uterm = '|';
		return TERMINAL;
	    case '{':
		return STARTCURLY;
	    case '.':
		c1 = yygetc();
		c2 = yygetc();
		if (c1 == '.' && c2 == '.')
		  return DOTS;
		else {
		  yyunget(c2);
		  yyunget(c1);
		  yylval.uterm = '.';
		  return TERMINAL;
		}
	    case '}':
		if (yypeek() == '+') {
		   yygetc();
		   return ENDCURLYPLUS;
		} else if (yypeek() == '*') {
		   yygetc();
		   return ENDCURLYMULT;
                } else {
		   yylval.uterm = '}';
		   return TERMINAL;
		}
	    default:
		yylval.uterm = c;
		return TERMINAL;
	}
}							/* for conctypes > */

static int
concsymbol(c)
register c;
{
    switch(c) {
    case '}' : return yypeek() != '+' && 
                      yypeek() != '*' ;

    case '.' : return !(yypeek() == '.' && yypeek2() == '.') ;

    case '|' : return !(yypeek() == ']' ||
                        (yypeek() == '|' && yypeek2() == ']'));

    default : return ispunct(c) && c != '{' && c != '<' && c != '^' ;
    }
}

static void
skipconcsymbols()
{
    register int c;

    do {               
         c = yygetc();
         if (c == '\\') {
	     yyp--;
	     yygetc();
         }
    } while (c != EOF && concsymbol(c));
    yyunget(c);
}

int
conclex2()			/* new lexical analysis for conctypes */
{
	register int c;

        while(isspace(yypeek()))     /* skip blanks */
		yygetc();
        yyp = yytext;
	c = yygetc();
	switch(c) {
	    case EOF:
		return EOF;
	    case '\\':                         
                yyp--;
                c = yygetc();
                if (ispunct(c)) {           /* look for symbols */
                  skipconcsymbols();
                  *yyp = 0;
                  yylval.uid = installid(yytext);
                  return SYMTERMINAL;
	        } else yyunget(c);
	        
	    case '^':
		if (yypeek()=='(') {
		    parcountstack[antiquoteflag] = parcount;
		    parcount = 0;
		    antiquoteflag++;
		    return ANTIQUOTE;
		}
		if (yylex1()==ID) {
		   return ANTIQVAR;
	        }
		yyerror("Expected an ident or ( after antiquote");
		return SYNTAX_ERROR;
	    case '<':
		nonterminalflag++;
		return LNONTERM;
            case '|':
		if (yypeek() == ']'){
		   yygetc();
		   concflag--;
		   return RQUOTE;
		}
		if (yypeek() == '|' && yypeek2() == ']'){
		   yygetc();
                   yygetc();
 	           concflag--;
		   return RFORGET;
		} else {                     /* look for symbols */
                   skipconcsymbols();
                   *yyp = 0;
                   yylval.uid = installid(yytext);
                   return SYMTERMINAL;
		  }
		   
	    case '{':
		return STARTCURLY;
	    case '.':
		if (yypeek() == '.' && yypeek2() == '.'){
                  yygetc();
                  yygetc();
		  return DOTS;
                }
		else {                     /* look for symbols */
                  skipconcsymbols();
                  *yyp = 0;
                  yylval.uid = installid(yytext);
                  return SYMTERMINAL;
                }
	    case '}':
		if (yypeek() == '+') {
		   yygetc();
		   return ENDCURLYPLUS;
		} else if (yypeek() == '*') {
		   yygetc();
		   return ENDCURLYMULT;
		} else {                     /* look for symbols */
                   skipconcsymbols();
                   *yyp = 0;
                   yylval.uid = installid(yytext);
                   return SYMTERMINAL;
		  }

	    case '0':
	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	    case '8':
	    case '9':
	        skipdigits();
  	        *yyp = 0;
	        yylval.uuint = atoi((char *)yytext);
	        return INTTERMINAL;
	
            default:
                if (concsymbol(c)) {
                  skipconcsymbols();
                  *yyp = 0;
                  yylval.uid = installid(yytext);
                  return SYMTERMINAL;
                }
                else if (letter(c)) {
                  do { 
                       c = yygetc();
		  } while (c != EOF && letter(c));
                  yyunget(c);
                  *yyp = 0;
                  yylval.uid = installid(yytext);
                  return IDTERMINAL;
	        }
                  
		yylval.uterm = c;
		return TERMINAL;
	}
}							/* for conctypes > */

int
yylex()
{
    register int r;

    yyp = yytext;
    r = (!nonterminalflag && concflag>antiquoteflag ? conclex2() : yylex1());
    *yyp = 0;
    lasttoken = r;
    if (debug)
	printf("tok=%d(\"%s\",%d,%d,%d,%d), ", r, yytext,concflag,antiquoteflag,nonterminalflag,parcount);
    return r;
}

void
pushfile(f, s)
FILE *f;
int s;
{
    oldfiles[oldindex++] = yyin;
    yyin = f;
}

int
popfile()
{
    if (*tmpname) {
	unlink(tmpname);
	tmpname[0] = 0;
    }
    fclose(yyin);
    yyin = oldfiles[--oldindex];
    if (loadlist) {
	loadlist = ltl(loadlist);
	if (tlist(loadlist) == lnil)
	    loadlist = 0;
	else {
	    FILE *f;
	    strcpy(loadname, (char*)lhd(loadlist)); strcat(loadname, ".t");
	    f = fopen(loadname, "r");
	    loadname[strlen(loadname)-1] = 'o';
	    pushfile(f, loadname);
	    yylval.uid = installid(loadname);
	    return OLOAD;
	}
    }
    return LEOF;
}

void
switchto(s)
list s;
{
    FILE *f;
    int l;

    if (tlist(ltl(s)) == lnil) {
	strcpy(loadname, (char *)lhd(s));
	l = strlen(loadname);
	if (loadname[l-2] == '.') {
	    switch(loadname[l-1]) {
	    case 'm':
	    loadm:
		if ((f = fopen(loadname, "r")) == NULL) {
		    errmsg("Cannot open %s\n", loadname);
		    pushtoken = LEOF;
		} else {
		    /* to simple, sigh! pushfile(f); */
		    char buf[1024];
		    sprintf(tmpname, "/tmp/ilml%d", getpid());
		    sprintf(buf, "%s -C -I%s %s %s", cppname, incdir, loadname, tmpname);
		    system(buf);
		    fclose(f);
		    pushfile(fopen(tmpname, "r"), loadname);
		    pushtoken = MLOAD;
		    yylval.uid = installid(loadname);
		    loadlist = 0;
		}
		break;
	    case 'o':
	    loado:
#if 0
		if ((f = fopen(loadname, "r")) == NULL) {
		    errmsg("Cannot open %s\n", loadname);
		    pushtoken = LEOF;
		} else 
#endif
		  {
		    char loadn[2000];
#if 0
		    fclose(f);
#endif
		    strcpy(loadn,loadname);
		    set_suffix(loadn,".t");
		    if ((f = fopen(loadn, "r")) == NULL) {
			errmsg("Cannot open %s\n", loadn);
			pushtoken = LEOF;
		    } else {
			add_o(loadname);
			pushfile(f, loadname);
			pushtoken = OLOAD;
			yylval.uid = installid(loadname);
			loadlist = 0;
		    }
		}
		break;
	    
	    bad:
	    default:
		error("Bad file name %s\n", loadname);
		pushtoken = LEOF;
	    }
	} else {
	    struct stat sm, so;
	    int rm, ro;

	    if(loadname[l-3] == '.' && loadname[l-2] == 's' &&
	       loadname[l-1] == 'o') goto loado;

	    strcpy(loadname+l, ".m");
	    l += 2;
	    rm = stat(loadname, &sm);
	    loadname[l-1] = 'o';
	    ro = stat(loadname, &so);
	    if (ro == -1 || (unsigned long)sm.st_mtime > so.st_mtime) {
		loadname[l-1] = 'm';
		goto loadm;
	    } else {
		goto loado;
	    }
	}
    } else {
	list p;

	for(p = s; tlist(p) != lnil; p = ltl(p)) {
	    FILE *f;
	    strcpy(loadname, (char *)lhd(p));
	    l = strlen(loadname);
	    strcpy(loadname+l, ".t");
	    if ((f = fopen(loadname, "r")) == NULL) goto bad; else fclose(f);
#if 0
	    strcpy(loadname+l, ".o");
	    if ((f = fopen(loadname, "r")) == NULL) goto bad; else fclose(f);
#endif
	}
	strcpy(loadname, (char *)lhd(s)); strcat(loadname, ".t");
	f = fopen(loadname, "r");
	loadname[strlen(loadname)-1] = 'o';
	pushfile(f, loadname);
	pushtoken = OLOAD;
	yylval.uid = installid(loadname);
	loadlist = s;
    }
}

void
source(s)
char *s;
{
    char b[1024];
    FILE *f;

    strcpy(b, s);
    if ((f = fopen(b, "r")) == NULL) {
	errmsg("Cannot open %s\n", loadname);
    } else {
	pushfile(f, s);
    }
}

void
seteof()
{
    forcedeof = 1;
}

void
initlex()
{
    char *s;
    extern char *getenv();

    yyinit();
    s = getenv("LMLDIR");
    if (!s) {
	s = "/usr/local/lib/lmlc";
    }
    sprintf(incdir, "%s/lib/include", s);
}

void
pushextra(t)
int t;
{
    pushtoken = t;
}

void
syntaxerror()
{
    pushtoken = SYNTAX_ERROR;
}

void makefixop(s,m,n) char *s; int m; int n; {}

#include "include.h"
#include "suffix.h"
#include "curry.h"
#include "ctype.h"
#include <sys/types.h>
#include <sys/stat.h>

#include "proto.h"

void check13 PROTO((char *, int));
void pedchk PROTO((char *));
void dependencychk PROTO((void));
int depchk PROTO((char *,FILE*));

#define MAXINFIX 100
#define MAXISTR 1000

#define COMM 10000
#define PLCOMM 10001

extern id installid();
extern int interactive, interfacef, debug, pre_1_1, h1_3;
extern int pedantic, useascii;
extern char *copystring();

extern char loadname[];

int yylineno = 1;
int undefline;
extern int zyzflag, zyzid;
char *filename;
FILE *yyin;
struct {
    FILE *file;
    char *name;
    int curindent, yylineno, curchar, pedantic, localquant, allowuscore;
    unsigned char *savec;
} oldfiles[100];
int oldindex = 0;
static int pushtoken = -1;
static id pushuid;
static int forcedeof = 0;
static unsigned char *yyp;
static char tmpname[200] = "";
static char incdir[1024];
char empty_prel[1024];
unsigned char yytext[5120];
unsigned char qname[1024];
static unsigned char yybuf[5120];
static int saved = 0;
static unsigned char savechar[1024];
#define LASTMAX 10
static int curchar='\n';
struct {
    int chr, ind;
} lastvec[LASTMAX];
static int lastind = 0;
static int lasttoken = 0;

int tabstop = 8;
static int indentlevel, curindent;
#define I_HARD 1
#define I_SOFT 2
struct ind {
    int indent;
    int kind;
};
#define MAXINDENT 100
#define INITIND 0
static struct ind indtab[MAXINDENT] = { { INITIND, I_HARD } };
static int indindex = 1;
#define topkind (indtab[indindex-1].kind)
#define topindent (indtab[indindex-1].indent)

static struct ctrl {
    char *code;
    int ord;
} ctrls[] = {
{"a",  7}, { "b",  8}, { "t",  9}, { "n",  10}, {"f",  12}, {"r",  13}, {"v",  11},
{"'",'\''}, {"\"",'"'}, {"\\",'\\'},
{"NUL",0},  {"SOH",1},  {"STX",2},  {"ETX",3},  {"EOT",4},  {"ENQ",5},  {"ACK",6},  {"BEL",7},
{"BS", 8},  {"HT", 9},  {"LF", 10}, {"VT", 11}, {"FF", 12}, {"CR", 13}, {"SO", 14}, {"SI", 15},
{"DLE",16}, {"DC1",17}, {"DC2",18}, {"DC3",19}, {"DC4",20}, {"NAK",21}, {"SYN",22}, {"ETB",23},
{"CAN",24}, {"EM", 25}, {"SUB",26}, {"ESC",27}, {"FS", 28}, {"GS", 29}, {"RS", 30}, {"US", 31},
{"SP",32},  {"DEL",127},
{0,0} };

static struct yytable {
    char *name;
    short token;
} yytable[] = {
/*    { "as",     AS_KWD }, */
    { "do",     DO },
    { "case",	CASE },
    { "class",	CLASS },
    { "data",	DATA },
    { "default",	DEFAULT },
    { "deriving",	DERIVING },
    { "else",	ELSE },
    { "hiding",	HIDING },
    { "if",	IF },
    { "import",	IMPORT },
    { "infix",	INFIX },
    { "infixl",	INFIXL },
    { "infixr",	INFIXR },
    { "nonfix",	MNONFIX },
    { "instance",	INSTANCE },
    { "interface",INTERFACE },
    { "module",	MODULE },
    { "newtype",NEWTYPE },
    { "of",	OF },
    { "qualified",	QUALIFIED },
    { "renaming",	RENAMING },
    { "then",	THEN },
    { "to",	TO },
    { "type",	TYPE },
    { "where",	WHERE },
    { "let",      LET },
    { "in",       IN },

    { "load",	LOAD },
    { "source",	SOURCE },
    { "whatis",	WHATIS },
    { "quit",	THEEND },
    { "help",	HELP },
    { 0,0 }
};

struct yytable annottable[] = {
    { "LINE",	A_LINE },
    { "INLINE",	A_INLINE },
    { "ARITY",	A_ARITY },
    { "FRAMESIZE",	A_FRAMESIZE },
    { "STRICTNESS",	A_STRICTNESS },
    { "STRICT",	A_STRICT },
    { "SHARED",	A_SHARED },
    { "UNSHARED",	A_UNSHARED },
    { "SPARK",	A_SPARK },
    { "ENTRY",	A_ENTRY },
    { "NOEVAL",   A_NOEVAL },
    { "OVERLOAD", A_OVERLOAD },
    { "ENTRY",    A_ENTRY },
    { "FLAT",     A_FLAT },
    { "DERIVED",  A_DERIVED },
    { "FROMMODULE",A_FROMMODULE },
    { "SPECIALIZE", A_SPECIALIZE },
    { "specialize", A_SPECIALIZE },
    { "SPECIALISE", A_SPECIALIZE },
    { "specialise", A_SPECIALIZE },
    { "EVALED",   A_EVALED },
    { "NOTCHK",	 A_NOTCHK },
    { 0,0, }
};

#define MAXOPR 1000
struct opr {
    char *oprname;
    int oprtoken;
} oprs[MAXOPR] = {
{"quot", OPL7},
{"mod",	OPL7},
{"div",	OPL7},
{"rem",	OPL7},
{"elem", OPN4},
{"notElem", OPN4},
{0,0}
};

#define COMM 10000
static struct infix {
    char *iname;	/* first char is ignored */
    short ilen;
    short itoken;
} infixtab[MAXINFIX] = {
{"?{-:",	3,	ANNOT},
{"?{-#",	3,	NANNOT},
{"?#-}", 3,      ENDANNOT},

{"?-}",  2,      ENDANNOT},
{"?--",	2,	COMM},
{"?{-",	2,	PLCOMM},
{"?..",	2,	DOTDOT},
{"P->",	2,	ARROW},
{"?<-",	2,	LARROW},
{"?=>",	2,	DARROW},
{"?::",	2,	DCOLON},

{"_!!",	2,	OPL9},
{"_^^",	2,	OPR8},
{"_**",	2,	OPR8},
{"_\\\\",2,	OPN5},
{"_++",	2,	OPR5},
{"_/=",	2,	OPN4},
{"_>=",	2,	OPN4},
{"_<=",	2,	OPN4},
{"_==",	2,	OPN4},
{"_&&",	2,	OPR3},
{"_||",	2,	OPR2},
/*{"_:%",	2,	OPN7},*/

{"?\\",	1,	BACKSLASH},
{"?;",	1,	SEMI},
{"?,",	1,	COMMA},
{"?(",	1,	LPAR},
{"?)",	1,	RPAR},
{"?[",	1,	LBRA},
{"?]",	1,	RBRA},
{"?{",	1,	LCURL},
{"?}",	1,	RCURL},
{"?|",	1,	BAR},
{"?@",	1,	AS},
{"?=",	1,	EQ},
{"?~",	1,	TILDE},

{"_.",	1,	OPR9},
{"_^",	1,	OPR8},
{"_*",	1,	OPL7},
{"_/",	1,	OPL7},
{"_+",	1,	OPL6},
{"_-",	1,	MINUS},
{"_:",	1,	OPR5},
{"_>",	1,	OPN4},
{"_<",	1,	OPN4},
{"_$",  1,      OPR0},

{"?",	0,	0},
};
static int ninfix;		/* # of predefined operators */
static char infixstr[MAXISTR];
static char *infixp = infixstr;
#define mlinf (infixtab[0].ilen)	/* longest length */

int
nfixes()
{
    return ninfix;
}

#define BADESC "Bad character escape"

static char *
decode(p, ip, base)
char *p;
unsigned int *ip;
int base;
{
    static char b8[] = "01234567", b10[] = "0123456789", b16[] = "0123456789abcdefABCDEF";
    char *b = base == 8 ? b8 : base == 10 ? b10 : b16;
    char *q;
    unsigned int r, n;

    r = 0;
    while(*p && (q = strchr(b, *p))) {
	n = q-b;
	if (n >= 16) n -= 6;
	r = r*base + n;
	p++;
    }
    *ip = r;
    return p;
}

static char *
unidecode(p, ip)
char *p;
unsigned int *ip;
{
    static char *b16 = "0123456789abcdefABCDEF";
    char *q;
    unsigned int r, n, i;

    r = 0;
    for(i = 0; i < 4; i++) {
	q = strchr(b16, *p++);
	if (!q)
	    break;
	n = q-b16;
	if (n >= 16) n -= 6;
	r = r*16 + n;
    }
    *ip = r;
    return p;
}

static int
processbackslash(q, p, str)
char *p, *q;
int str;
{
    unsigned int c;
    struct ctrl *ct;
    char *oq = q;
    int spec = 0, uni = 0;

    while(*p) {
	if ((c = (*p++ & 0xff)) == '\\') {
	    switch(c = *p++) {
	    case '&': continue;
	    case '^': c = *p++; if (c < '@' || c > '_') yyerror(BADESC); c &= 0x1f; break;
	    case 'o': /* Octal */
		p = decode(p, &c, 8);
		break;
	    case 'x': /* Hex */
		p = decode(p, &c, 16);
		break;
	    case 'u': /* Unicode hex */
		if (!(isxdigit(p[0]) && isxdigit(p[1]) && isxdigit(p[2]) && isxdigit(p[3])))
		    yyerror(BADESC);
		p = unidecode(p, &c);
		uni = 1;
		break;
	    case '0':case'1':case'2':case'3':case'4':case'5':case'6':case'7':case'8':case'9':
		/* Decimal */
		p = decode(p-1, &c, 10);
		break;
	    case '\'': break;
	    case '"': break;
	    case '\\': break;
	    case ' ': case '\t':
		while(*p == ' ' || *p == '\t')
		    p++;
		if (*p++ != '\n')
		    yyerror(BADESC);
	    case '\n':
		while(*p == ' ' || *p == '\t')
		    p++;
		if (*p++ != '\\')
		    yyerror(BADESC);
		continue;
	    default:
		c = -1;
		p--;
		for(ct = ctrls; ct->code; ct++) {
		    if (strncmp(p, ct->code, strlen(ct->code)) == 0) {
			c = ct->ord;
			p += strlen(ct->code);
			break;
		    }
		}
		if (c == -1)
		    yyerror(BADESC);
		break;
	    }
	}
	if (uni) {
	    sprintf(q, "\\u%04x", c);
	    q += 6;
	    spec += 5;
	    uni = 0;
	} else {
	    if (c > 255)
		yyerror("Character value > 255");
	    /* Change all tabs back to \t, \ to \\, and NUL to \0 */
	    if (str) {
		if (c == '\t')
		    *q++ = '\\', *q++ = 't', spec++;
		else if (c == '\\')
		    *q++ = '\\', *q++ = '\\', spec++;
		else if (c == '\0')
		    *q++ = '\\', *q++ = '0', spec++;
		else
		    *q++ = c;
	    } else
		*q++ = c;
	}
    }
    *q = 0;
    return q - oq - spec;
}

static int
letter(c)
int c;
{
    return ((!useascii || c < 0x80) && isalnum(c)) || c == '_' || c == '\'';
}

static int
allletter(s)
char *s;
{
    for(;*s;s++)
	if (!letter(*s))
	    return 0;
    return 1;
}

void
makeopr(ss, t)
char *ss;
int t;
{
    struct opr *op;

    for(op = oprs; op->oprname; op++)
	;
    if (op - oprs >= MAXOPR)
	error("Too many oprs");
    op->oprname = installid(ss);
    op->oprtoken = t;
    op++;
    op->oprname = 0;
}

void
makeinfix(ss, token)
char *ss;
int token;
{
    int i, l;
    char s[1000];

    strcpy(s, ss);
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
makefixop(ss, ass, level)
char *ss;
int ass, level;
{
    static int non[] = { OPN0,OPN1,OPN2,OPN3,OPN4,OPN5,OPN6,OPN7,OPN8,OPN9 };
    static int left[] = { OPL0,OPL1,OPL2,OPL3,OPL4,OPL5,OPL6,OPL7,OPL8,OPL9 };
    static int right[] = { OPR0,OPR1,OPR2,OPR3,OPR4,OPR5,OPR6,OPR7,OPR8,OPR9 };
    int t;

    switch(ass) {
    case INFIX: t = non[level]; break;
    case INFIXL: t = left[level]; break;
    case INFIXR: t = right[level]; break;
    case NONFIX: t = ass; break;
    default: fprintf(stderr, "Bad makefixop\n"); exit(1); break;
    }
    if (allletter(&ss[1])) {
	makeopr(ss+1, t);
    } else {
	makeinfix(ss, t);
    }
}

void
makefixopl(l, a, v)
list l;
int a, v;
{
    for(; tlist(l) != lnil; l = ltl(l))
	makefixop((char *)lhd(l), a, v);
}

int
lookupopr(s)
char *s;
{
    struct opr *op;

    for(op = oprs; op->oprname; op++)
	if (strcmp(op->oprname, s) == 0)
	    return op->oprtoken;
    return OPL9;
}

void
yyinit()
{
    yyin = stdin;
    for(ninfix = 0; infixtab[ninfix].ilen; ninfix++)
	;
    ninfix++;
    if (h1_3) {
	makeinfix("_>>", OPL1);
	makeinfix("_>>=", OPL1);
	makeopr("seq", OPR0);
    } else {
	makeinfix("_:+", OPN6);
	makeinfix("_:=", OPN1);
	makeinfix("_%",	 OPL7);
	makeinfix("_!",	 OPL9);
	makeinfix("_//", OPL9);
    }
    saved = 0;
    yylineno = 1;
    lastvec[lastind % LASTMAX].chr = '\n';
    lastvec[lastind % LASTMAX].ind = 0;
}

static struct yytable *
lookup(name, tab)
char *name;
struct yytable *tab;
{
    struct yytable *p;

    for(p = tab; p->name; p++)
	if (strcmp(name, p->name) == 0 && (p->token != NONFIX || zyzflag))
	    return p;
    return 0;
}

static int
white(c)
int c;
{
    return c == '\n' || c == '\t' || c == ' ' || c == '\f' || c == '\013' || c == '\r';
}

static int
isoperator(c, first)
int c;
int first;
{
    static char ops[] =  "!#$%&*+./<=>?@\\^|:~-";
    static char pops[] = "!#$%&*+./<=>?@\\^|:";
    static char iso_ops[] = "\241\242\243\244\245\246\247\250\251\252\253\254\255\256\257\260\261\262\263\264\265\266\267\270\271\272\273\274\275\276\277\327\367";

    return strchr(first || !pedantic || h1_3 ? ops : pops, c) || (!useascii && strchr(iso_ops, c));
}

static int
alloperator(p)
char *p;
{
    if (!isoperator(*p++, 1))
	return 0;
    while(*p)
	if (!isoperator(*p++, 0))
	    return 0;
    return 1;
}

static int
yygetc()
{
    int c;
    static int seeneof = 0;

    lastvec[++lastind % LASTMAX].chr = curchar;
    lastvec[lastind % LASTMAX].ind = curindent;
    if (saved > 0) {
	c = savechar[--saved] & 0xff;
    } else {
	if (seeneof)
	    c = EOF;
	else
	    c = getachar(interactive,yyin);
    }
    if (c == '\n') {
	yylineno++;
	curindent = 0;
    } else if (c == '\t') {
	curindent = (curindent / tabstop + 1) * tabstop;
    } else {
	curindent++;
    }
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
	curchar = lastvec[lastind % LASTMAX].chr;
	curindent = lastvec[lastind-- % LASTMAX].ind;
    }
}

static int
yypeek()
{
    int c;

    c = yygetc();
    yyunget(c);
    return c;
}

static int
yypeek2()
{
    int c, r;

    c = yygetc();
    r = yypeek();
    yyunget(c);
    return r;
}

static void
skipdigits()
{
    while(isdigit(yypeek()))
	(void)yygetc();
}

static int
unichar()
{
    char u = yygetc(), 
         c1 = yygetc(), 
         c2 = yygetc(), 
         c3 = yygetc(),
         c4 = yygetc();
    if (u == 'u' && isxdigit(c1) && isxdigit(c2) && isxdigit(c3) && isxdigit(c4)) {
        int c;
        char b[5];
        b[0] = c1;
        b[1] = c2;
        b[2] = c3;
        b[3] = c4;
        b[4] = 0;
        sscanf(b, "%x", &c);
        return c;
    }
    yyunget(c4);
    yyunget(c3);
    yyunget(c2);
    yyunget(c1);
    yyunget(u);
    return -1;
}

#define UNICODEFILE "UnicodeInfo"

static void
uniload()
{
  static int loaded = 0;
  extern char *hlibname;
  char file[2000];

  if (loaded)
    return;
  loaded = 1;
  sprintf(file, "%s/%s", hlibname, UNICODEFILE);
  if (readUniFile(file) < 0) {
    fprintf(stderr, "Couldn't read Unicode table\n");
  }
}

static int
uniletter(c)
int c;
{
    if (letter(c))
        return 1;
    if (c == '\\' && yypeek() == 'u') {
        c = unichar();
        if (c < 0)
            return 0;
        uniload();
        return uni_isalpha(c);
    }
    return 0;
}

static int
uniisupper(p)
char *p;
{
    if (isupper(*p))
        return 1;
    if (p[0] == '\\' && p[1] == 'u') {
        int c;
        char b[5];
        strncpy(b, p+2, 4);
        b[4] = 0;
        sscanf(b, "%x", &c);

        uniload();
        return uni_isupper(c);
    }
    return 0;
}

static void
pushindent(k)
int k;
{
    if (debug) {
	printf("pushindent(%s,ind=%d,ix=%d) ", k==I_SOFT?"soft":"hard", indentlevel, indindex);
	fflush(stdout);
    }
    if (indindex >= MAXINDENT)
	error("Too many indent levels\n");
    indtab[indindex].indent = indentlevel;
    indtab[indindex++].kind = k;
}

static int
popindent(k)
int k;
{
    int r;

    if (--indindex < 1) {
/*	error("Too many popindent\n"); */
	indindex = 1;
	return SYNTAX_ERROR;
    }
    if (indtab[indindex].kind != k)
	r = SYNTAX_ERROR;
    else
	r = RCURL;
    if (debug) {
	printf("popindent(%s,ind=%d,cur=%d,ix=%x)=%d ", k==I_SOFT?"soft":"hard", indtab[indindex].indent, indentlevel, indindex, r);
	fflush(stdout);
    }
    return r;
}

void
popsoft()
{
    if (popindent(I_SOFT) != RCURL) {
	yyerror("Syntax error");
    }
}

#define isodigit(c) ( '0' <= (c) && (c) <= '7')

int 
iso_isupperp(p) 
char *p; 
{ 
    while(*p && *p=='_') p++; 
    return uniisupper(p);
}

/* should skip comments as well */
static void
skipwhite()
{
    while(white(yypeek())) {
	(void)yygetc();
	yyp--;
    }
}

extern int allowtypeexcl, allowas, allowstar, localquant, allowuscore, view;

static int
yylex1()
{
    int i, j, c;
    struct yytable *p;
    int t;
    unsigned char ibuff[1000];
    char tt[1000];
    static int inannot = 0;

    if (pushtoken >= 0) {
	t = pushtoken;
	pushtoken = -1;
	if (pushuid) {
	    yylval.uid = pushuid;
	    pushuid = 0;
	}
	return t;
    }
    if (forcedeof)
	return EOF;
    for (;;) {
    again:
	while(white(yypeek())) {
	    if (yygetc() == '\n' && interactive && !oldindex) {
		if (lasttoken != SEMI && lasttoken != LEOF && lasttoken != ERREOF) {
		    secprompt();
		}
	    }
	}

	if (curchar == '\n' && yypeek() == '#') {
	    char buf[1000];
	    yyp = yytext;
	    while((c = yygetc()) != '\n' && c != EOF)
		;
	    *yyp = 0;
	    if (sscanf((char *)&yytext[1], "%d \"%[^\"]", &yylineno, buf) < 1) {
		if (sscanf((char *)&yytext[1], "line %d \"%[^\"]", &yylineno, buf) < 1) {
		    fprintf(stderr, "Bad # in source: %s\n", yytext);
		    exit(1);
		}
	    }
#if 0
Ignore this, wrong file name if literal.
	    filename = copystring(buf);
#endif
	    goto again;
	}

	indentlevel = curindent;
	yyp = yytext;

        /* check for unicode letter first */
        c = yygetc();
        if (c == '\\' && uniletter(c))
            goto doletter;
        yyunget(c);

    	/* first try for an infix operator */
	for(i = 0; i < mlinf && yypeek() != '\n' && yypeek() != EOF; i++) {
	    ibuff[i] = yygetc();
	    if (h1_3 && i > 0 && ibuff[i-1] != '-' && ibuff[i] == '-' && yypeek() == '-' && !(ibuff[0] == '{' && ibuff[1] == '-')) {
		yyunget(ibuff[i]);
		ibuff[i] = 0;
		goto chk;		/* we're at a comment */
	    }
	}
	ibuff[i] = 0;
	if (alloperator(ibuff) && isoperator(yypeek(), 0)) {
	    /* More chars will make up a longer operator */
	    do {
		ibuff[i++] = yygetc();
	    } while(isoperator(yypeek(), 0));
	    ibuff[i] = 0;
	    *yyp = 0;
	    if (ibuff[0] == '-' && ibuff[1] == '-') {
		t = COMM;
		goto comment;
	    }
	    t = OPL9;
	    goto identi;
	}
    chk:
	if (ibuff[0] == '-' && ibuff[1] == '-') {
	    t = COMM;
	    goto comment;
	}
	for(j = 0; j < ninfix; j++) {
	    while (infixtab[j].ilen < i) {
		/* Before chopping off last char of potential operator check if it is
		   a valid operator on its own. */
		if (alloperator(ibuff)) {
		    /* All chars are valid operator chars. */
		    *yyp = 0;
		    t = OPN9;
		    goto identi;
		}
	        yyunget(ibuff[--i]);
		ibuff[i] = 0;
	    }
	    if (h1_3 && allowtypeexcl && strcmp((char *)ibuff, "!") == 0)
		return TYPEEXCL;
	    if (h1_3 && allowstar && strcmp((char *)ibuff, "*") == 0)
		return STAR;
	    if (h1_3 && allowtypeexcl && strcmp((char *)ibuff, "?") == 0 &&
		islower(yypeek())) {
	        /* Special existential tyvar */
	        do {
		    c = yygetc();
		} while(letter(c));
		yyunget(c);
		*yyp = 0;
		yylval.uid = installid(yytext);
		return NAME;
	    }
	    if (i == 0)
	    	break;
	    if (strcmp((char *)ibuff, infixtab[j].iname+1) == 0) {
	    	t = infixtab[j].itoken;
/*printf("opr '%s'=%d ", infixtab[j].iname, t);*/
		if (t == COMM) {
		comment:
		    /* Skip the comment */
		    while ((c = yygetc()) != '\n' && c != EOF)
		        ;
		    yyunget(c);
		    goto again;
		} else if (t == PLCOMM) {
		    int nest;

		skipcomm:
		    for(nest = 1; nest; nest--) {
			do {
			    while((c = yygetc()) != '-' && c != EOF) {
				if (c == '{' && yypeek() == '-')
				    nest++;
				yyp = yytext;
			    }
			} while (yypeek() != '}' && yypeek() != EOF);
			(void)yygetc();
		    }
		    goto again;
		} else if (t == ANNOT) {
		    if (!zyzid)
			goto skipcomm;
		    while(!(yygetc() == ':' && yypeek() == '-'))
			;
		    yygetc(), yygetc();
		    yyp[-3] = 0;
		    if (zyzid && yytext[3] == '"') {
			/* special id */
                        char *p = strrchr((char *)yytext+4, '"');
                        if (p == 0) {
                            yyerror("Missing \" in {-:");
                        } else {
                            *p = 0;
                            processbackslash(yybuf, yytext+4, 0);
                            yylval.uid = installid(yybuf);
                            return yyp[-4] == 'C' ? CNAME : NAME;
                        }
		    } else if (zyzid && (yytext[3] == '#')) {
			yylval.uuint = atoi((char *)yytext+4);
			return INTCONST;
		    } else {
			yylval.uid = installid(yytext+3);
			return ANNOT;
		    }
		} else if (t == NANNOT) {
		    inannot++;
		    while(isspace(yypeek()))
			yygetc();
		    yyp = yytext;
		    do {
			c = yygetc();
		    } while(c != EOF && letter(c));
		    yyunget(c);
		    *yyp = 0;
		    p = lookup(yytext, annottable);
		    if (!p) {
			/* Not recognised, skip it */
			goto skipcomm;
		    }
		    if (p->token == A_LINE) {
			int t, no;
			t = yylex1();
			if (t != DIGIT && t != INT)
			    goto skipcomm;
			no = atoi(yylval.uid);
			t = yylex1();
			if (t != STRING)
			    goto skipcomm;
			yylineno = no;
			filename = yylval.uid;
			goto skipcomm;
		    }
		    return p->token;
		} else if (t == ENDANNOT) {
		    if (!inannot)
			yyerror("Spurious -}");
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
		return LEOF;
	    } else
		if (interactive) {
		    interactive = 0;
		    return THEEND;
		} else
		    return EOF;
	case ' ': 
	case '\t': 
	case '\n':
	    yyerror("illegal white space");
	    break;

	case '\'':
	    do {
		c = yygetc();
		if (c == EOF || c == '\n')
		    yyerror("Nonterminated character");
		if (c == '\\')
		    if (yygetc() == EOF)
			yyerror(BADESC);
	    } while (c != '\'');
	    *--yyp = 0;
	    yybuf[1] = 1;
	    if (processbackslash(yybuf, yytext+1, 0) != 1)
		return SYNTAX_ERROR;
	    yylval.uid = installid(yybuf); 
	    return CHAR;
	case '"':
	    do {
		c = yygetc();
		if (c == EOF || c == '\n')
		    yyerror("Nonterminated string");
		if (c == '\\')
		    if (yygetc() == EOF)
			yyerror(BADESC);
	    } while (c != '\"');
	    *--yyp = 0;
	    processbackslash(yybuf, yytext+1, 1);
	    yylval.uid = installid(yybuf);
	    return STRING;
	case '0':
	    if (!pedantic || h1_3) {
		c = yypeek();
		if (c == 'x' || c == 'X') {
		    /* hex number */
		    yygetc();	/* 'x' */
		    if (!isxdigit(yypeek()))
			return SYNTAX_ERROR;
		    do {
			c = yygetc();
		    } while(isxdigit(c));
		    yyunget(c);
		    *yyp = 0;
		    yytext[1] = 'x';
		    yylval.uid = installid(yytext+1);
		    return INT;
		} else if (c == 'o' || c == 'O') {
		    /* octal number */
		    yygetc();	/* 'o' */
		    if (!isodigit(yypeek()))
			return SYNTAX_ERROR;
		    do {
			c = yygetc();
		    } while(isodigit(c));
		    yyunget(c);
		    *yyp = 0;
		    yytext[1] = 'o';
		    yylval.uid = installid(yytext+1);
		    return INT;
		}
	    }
	    /* fall into */
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
	    c = yypeek();
	    if ((c == '.' && isdigit(yypeek2())) || (!pedantic && (c == 'e' || c == 'E'))) {
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
		yylval.uid = installid(yytext);
		return RATNUM;
	    } else {
		*yyp++ = 'I';
		*yyp = 0;
		yylval.uid = installid(yytext);
		return yytext[1] == 'I' ? DIGIT : INT;
	    }
	case '`':
	    {
		unsigned char *p;
		int dotcnt = 0;
		skipwhite();
		p = yyp = yytext;
		do {
		    c = yygetc();
		    if (c == '.') {
			p = yyp;
			dotcnt++;
		    }
		} while(letter(c) || (h1_3 && c == '.' && dotcnt == 1 && iso_isupperp(yytext)));
		while(white(c))
		    c = yygetc();
		if (c != '`')
		    return SYNTAX_ERROR;
		yyp[-1] = 0;
		if (lookup(p, yytable))
		    return SYNTAX_ERROR; /* Don't allow keywords to be operators */
		t = lookupopr(p);
		goto identi;
	    }
	case '_':
	    if (!letter(yypeek())) {
		*yyp = 0;
		yylval.uid = "_";
		return WILD;
	    }
		/* fall into ... */
	default:
	    if (letter(c)) {
            doletter:
		do {
		    c = yygetc();
		} while(uniletter(c));
		if (h1_3 && c == '.' && iso_isupperp(&yytext[zyzflag]) && (letter(yypeek()) || isoperator(yypeek(), 1)) ) {
		    /* A qualified name */
		    *--yyp = 0;	/* ignore . */
		    strcpy((char *)qname, (char *)yytext); /* save qualified part */
		    return QNAME;
		}
		yyunget(c);
		*yyp = 0;
		if ((p = lookup(yytext, yytable))) {
		    if (!interactive && p->token >= LOAD)
			goto identif;
		    if (pre_1_1) {
			if (p->token == WHERE)
			    return OLD_WHERE;
			if (p->token == LET || p->token == IN)
			    goto identif;
		    } else if (!h1_3) {
			if (p->token == DO || p->token == NEWTYPE || p->token == QUALIFIED)
			    goto identif;
		    } else {
			/* Haskell 1.3 */
			if (p->token == RENAMING || p->token == TO)
			    goto identif;
		    }
		    return p->token;
		} else {
		    if (yytext[0] == '_' && !allowuscore)
			yyerror("leading '_' in identifier");
		    if (h1_3 && allowas) {
			if (strcmp((char *)&yytext[zyzflag], "as") == 0)
			    return AS_KWD;
#if 0
			if (strcmp((char *)&yytext[zyzflag], "hiding") == 0)
			    return HIDING;
			if (strcmp((char *)&yytext[zyzflag], "qualified") == 0)
			    return QUALIFIED;
#endif
		    }
                    if (view)
			if (strcmp((char *)&yytext[zyzflag], "view") == 0)
			    return VIEW;
		    if (h1_3 && strcmp((char *)yytext, "undefined") == 0)
		      undefline = yylineno;
		identif:
		    t = iso_isupperp(&yytext[zyzflag]) ? CNAME : NAME;
		identi:
		    sprintf(tt, "%s%s", zyzflag ? "" : "_", yytext);
		    yylval.uid = installid(tt);
		    return t;
		}
	    } else if (isoperator(c, 1)) {
		do {
		    c = yygetc();
		} while(isoperator(c, 0));
		yyunget(c);
		*yyp = 0;
		t = OPN9;
		goto identi;
	    } else {
		return SYNTAX_ERROR;
	    }
	}
    }
}

int
yylex2()
{
    int r;
    static int first = 1;
    static int ptoken = -10;
    static int sawview = 0;
    static int okqtoken[] = { NAME, CNAME, MINUS,
	                      OPN0,OPN1,OPN2,OPN3,OPN4,OPN5,OPN6,OPN7,OPN8,OPN9,
			      OPL0,OPL1,OPL2,OPL3,OPL4,OPL5,OPL6,OPL7,OPL8,OPL9,
			      OPR0,OPR1,OPR2,OPR3,OPR4,OPR5,OPR6,OPR7,OPR8,OPR9,
			      -1 };

    if (ptoken >= -1) {
	r = ptoken;
	ptoken = -10;
	return r;
    }
    yyp = yytext;
    r = yylex1();
    if (r == VIEW)
	sawview++;
    *yyp = 0;
    if (r == QNAME) {
	int i;
	int ind = indentlevel;
	r = yylex1();
	indentlevel = ind;
	*yyp = 0;
	for(i = 0; okqtoken[i] >= 0 && r != okqtoken[i]; i++)
	    ;
	if (okqtoken[i] < 0)
	    yyerror("Bad qualified name");
	sprintf((char *)yytext, "_%s.%s", qname, yylval.uid+1);
	yylval.uid = installid(yytext);
    }
    if (first && !interactive && !interfacef) {
	/* generate an LCURL if short module form */
	first = 0;
	if (r != MODULE && r != LCURL) {
	    if (debug) {
		printf("abbr-module=%d, ", LCURL);
		fflush(stdout);
	    }
	    pushindent(I_SOFT);
	    ptoken = r;
	    return LCURL;
	}
    }
    first = 0;
    if (r == LCURL) {
	pushindent(I_HARD);
	return r;
    }
    if (topkind == I_SOFT) {
	if (r == EOF || r == LEOF)
	    indentlevel = -1;
	if (indentlevel == topindent) {
	    ptoken = r;
	    if (debug) {
		printf("same-indent=%d ", SEMI);
		fflush(stdout);
	    }
	    return SEMI;
	} else if (indentlevel < topindent) {
	    if (debug) {
		printf("lt-indent ");
		fflush(stdout);
	    }
	    pushtoken = r;
	    return popindent(I_SOFT);
	}
    }
    if (r == RCURL)
	return popindent(I_HARD);
    if (lasttoken == OF && sawview) {
	sawview = 0;
	return r;
    }
    if ((lasttoken == WHERE || lasttoken == OF || lasttoken == LET || lasttoken == DO) && r != LCURL) {
	if (debug) {
	    printf("soft-curl(%d,%d)=%d ", lasttoken, r, LCURL);
	    fflush(stdout);
	}
	pushindent(I_SOFT);
	ptoken = r;
	return LCURL;
    }   
    return r;
}

int
yylex()
{
    int r;
    extern char *clexeme();

    r = yylex2();
    if (r == LEOF)
	popfile();

    lasttoken = r;
    if (debug) {
	printf("'%s'=%s(%d), ", yytext, clexeme(r), r);
	fflush(stdout);
    }
    return r;
}

void
pushfile(f, s)
FILE *f;
char *s;
{
    oldfiles[oldindex].yylineno = yylineno;
    oldfiles[oldindex].curindent = curindent;
    oldfiles[oldindex].curchar = curchar;
    oldfiles[oldindex].name = filename;
    oldfiles[oldindex].pedantic = pedantic;
    oldfiles[oldindex].localquant = localquant;
    oldfiles[oldindex].allowuscore = allowuscore;
    filename = copystring(s);
    if (saved > 0) {
	savechar[saved] = 0;
	oldfiles[oldindex].savec = (unsigned char *)copystring(savechar);
	saved = 0;
    } else {
	oldfiles[oldindex].savec = 0;
    }
    localquant = 1;
    allowuscore = 1;
    pedantic = 0;
    curchar = '\n';
    curindent = 0;
    yylineno = 1;
    zyzid++;
    indentlevel = INITIND;
    pushindent(I_HARD);
    oldfiles[oldindex++].file = yyin;
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
    yyin = oldfiles[--oldindex].file;
    curindent = oldfiles[oldindex].curindent;
    yylineno = oldfiles[oldindex].yylineno;
    curchar = oldfiles[oldindex].curchar;
    pedantic = oldfiles[oldindex].pedantic;
    localquant = oldfiles[oldindex].localquant;
    allowuscore = oldfiles[oldindex].allowuscore;
    /*free(filename); Don't free! It may be used in the errinfo table */
    filename = oldfiles[oldindex].name;
    zyzid--;
    if (oldfiles[oldindex].savec) {
	strcpy((char *)savechar, (char *)oldfiles[oldindex].savec);
	saved = strlen((char *)savechar);
	free(oldfiles[oldindex].savec);
    } else
	saved = 0;
    return popindent(I_HARD);
}


void
switchto(s)
char *s;
{
    FILE *f;
    int l;

    strcpy(loadname, s);
    l = strlen(loadname);
    if (loadname[l-3] == '.' && loadname[l-2] == 'h' && loadname [l-1] == 's') {
    loadm:
	if ((f = fopen(loadname, "r")) == NULL) {
	    errmsg("Cannot open %s\n", loadname);
	    pushtoken = ERREOF;
	} else {
	    pushfile(f, loadname);
	    pushuid = installid(loadname);
	    pushtoken = MLOAD;
	}
    } else if ((loadname[l-2] == '.' ||
	        loadname[l-3] == '.' && loadname[l-2] == 's') && 
	        loadname[l-1] == 'o') {
    loado:
#if 0
	if ((f = fopen(loadname, "r")) == NULL) {
	    errmsg("Cannot open %s\n", loadname);
	    pushtoken = ERREOF;
	} else
#endif
	  {
	    char loadn[2000];
	    int i;
#if 0
	    fclose(f);
#endif
	    strcpy(loadn,loadname);
	    i = loadn[l-2] == '.' ? l : l-1;
	    loadn[i-1] = 'h';
	    loadn[i] = 'i';
	    loadn[i+1] = 0;
	    if ((f = fopen(loadn, "r")) == NULL) {
		errmsg("Cannot open %s\n", loadn);
		pushtoken = ERREOF;
	    } else {
		pushfile(f, loadname);
		pushuid = installid(loadname);
		pushtoken = OLOAD;
	    }
	}
    } else {
	struct stat sm, so;
	int rm, ro;

	strcpy(loadname+l, ".hs");
	l += 3;
	rm = stat(loadname, &sm);
	add_o(loadname);
	ro = stat(loadname, &so);
	if (ro == -1 || (unsigned long)sm.st_mtime > so.st_mtime) {
	    loadname[l-2] = 'h';
	    loadname[l-1] = 's';
/*	    normmsg("Loading \"%s\"\n", loadname);*/
	    goto loadm;
	} else {
	    l--;
	    goto loado;
	}
    }
}

void
switchtomod(mods)
list mods;
{
    char *mod;
    struct stat so;

    if (tlist(ltl(mods)) != lnil) {
      fprintf(stderr, "Multiple imports not implemented yet.\n");
      pushtoken = ERREOF;
    } else {
      mod = (char *)lhd(mods);
      if (!switchtoid(mod)) {
	pushtoken = ERREOF;
      } else {
	add_o(loadname);
	if (stat(loadname, &so) >= 0)
	  dependencychk();
	else {
	  extern list autoloads,Lnil;
	  char loadn[2000];
#if 0
	  strcpy(loadn,loadname);
	  set_suffix(loadname,"/lib_i.so");
	  if(stat(loadname,&so) < 0) {
	    set_suffix(loadname,"/lib.so");
	    if(stat(loadname,&so) < 0) {
	      popfile();
	      pushtoken = ERREOF;
	      errmsg("Cannot open %s\n", loadn);
	      return;
	    }
	  }
#endif
	  autoloads = Lnil;
	}
	pushuid = installid(loadname);
	pushtoken = OLOAD;
      }
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
	pushfile(f, b);
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
    s = getenv("HBCDIR");
    if(!s)
      s = getenv("LMLDIR");
    if (!s) {
	s = "/usr/local/lib/lmlc";
    }
    sprintf(incdir, "%s/lib/include", s);
    sprintf(empty_prel, "_%s/hlib/PreludeEmpty", s);
}

void
syntaxerror()
{
    pushtoken = SYNTAX_ERROR;
}

void
pushextra(t)
int t;
{
    pushtoken = t;
}

void
pushlast()
{
    pushtoken = lasttoken;
}

int
fixtype(n)
int n;
{
    fprintf(stderr, "fixtype\n");
    exit(1);
}

char *
fixop(n)
int n;
{
    fprintf(stderr, "fixop\n");
    exit(1);
}

void
pedchk(s)
char *s;
{
    if (pedantic) {
	char buf[200];
	sprintf(buf, "Not allowed in pedantic mode: %s", s);
	yyerror(buf);
    }
}

void
check13(s, f)
char *s;
{
    if (f != h1_3) {
	char buf[200];
	sprintf(buf, "%s allowed in Haskell 1.3: %s", f ? "Only" : "Not", s);
	yyerror(buf);
    }
}

#define IMPORTING "{-# IMPORTING"
#define IMPORTINGLEN 13

struct depfile {
  char *name;
  list children;
  struct depfile *next;
} *depfiles;

/* return negative index if already in depfiles */ 
int
adddep(s,childl)
char *s;
list **childl;
{
    struct depfile *p;
    int i;
    
    for(i = 1, p = depfiles; p; i++, p = p->next)
      if (strcmp(p->name, s) == 0) {
	int j;
	for(j=0; p; j++)
	  p = p->next;
	return -j;
      }
    p = (struct depfile *)malloc(sizeof(struct depfile));
    p->name = copystring(s);
    p->next = depfiles;
    p->children = mklnil();
    *childl = &(p->children);
    depfiles = p;
    return i;
}

/* Check what the loaded file depends on, and issue load commands for these */
void
dependencychk()
{
    char tmp[2000];
    int l = strlen(loadname);
    list dl;
    struct depfile *p;
    extern list Lnil;
    extern list autoloads;

    depfiles = 0;
    strcpy(tmp, loadname);
    loadname[l-1] = 'h'; loadname[l] = 'i'; loadname[l+1] = 0;
    depchk(loadname, yyin);
    strcpy(loadname, tmp);

/*    fprintf(stderr, "Depends on");*/
    for(dl = Lnil, p=depfiles; p && p->next; p=p->next) {
        add_o(p->name);
	dl = mklcons(mkppair(p->name,p->children), dl);
/*	fprintf(stderr, " %s", p->name);*/
    }
/*    fprintf(stderr, "\n");*/
/*    if (tlist(dl) == lcons)
	picmd(mkIoload(dl)); */
    autoloads = dl;
}

int
depchk(lname, f)
char *lname;
FILE *f;
{
    char line[1000];
    char *name, *p, nxt;
    FILE *newf;
    extern char *incpath;
    extern FILE *pathopen();
    char buf[1000];
    list *childl;
    int myno = adddep(lname,&childl);

    if(myno<0) return -myno;

    fgets(line, sizeof line, f); /* skip "interface ..." */
    fgets(line, sizeof line, f); /* potential IMPORTING */
    rewind(f);
    /*fprintf(stderr, "depchk %s: %s\n", lname, line);*/
    if (strncmp(line, IMPORTING, IMPORTINGLEN) != 0)
	return myno;			/* no IMPORTING */
    p = line + IMPORTINGLEN;
    for(;;) {
	while(isspace(*p))
	    p++;
	name = p;
	while(letter((unsigned char)*p))
	    p++;
	nxt = *p;
	*p++ = 0;
	strcpy(buf, name); strcat(buf, ".hi");
	newf = pathopen(incpath, buf, "r");
	if (newf) {
	    int childno = depchk(loadname, newf);
	    *childl = mklcons(childno, *childl);
	    fclose(newf);
	}
	if (nxt != ',')
	    break;
    }
    return myno;
}

void
getout()
{
    if (!interactive)
	exit(1);
    pushtoken = ERREOF;
}

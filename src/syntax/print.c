#include <stdio.h>
#include <varargs.h>
#include "include.h"
#include "listgen.h"

#include "proto.h"

void ptree PROTO((tree t));
void pttype PROTO((ttype t));
void pimpstuff PROTO((impstuff p, void (*f)()));
void prbind PROTO((binding b));
void pfixes PROTO((void));
void pimpid PROTO((impidt i));
void ppbinding PROTO((pbinding p));
void pqual PROTO((qual q));
void prexpid PROTO((expidt e));
void pfinfo PROTO((finfot f));
void patype PROTO((atype a));

tree niltree;
id listid;
list Lnil;
int warnflag = 1;
int zyzflag = 0;
int zyzid = 0;
extern char *progname;
extern int curryflag;
char *delimiter = "";
int skipping = 0;
extern int h1_3;

#define SP delimiter

void
init()
{
    niltree = mkident(installid("_[]"));
    Lnil = mklnil();
    listid = installid(h1_3 ? "_[]" : "PList");
}

void
picmdmsg(s)
char *s;
{
    printf("`%sS%s#%s\t", SP, SP, s);
    fflush(stdout);
}

void
picmdmsgnp(s)
char *s;
{
    printf("/%sS%s#%s\t", SP, SP, s);
    fflush(stdout);
}

void
plist(fun, l)
void (*fun)();
list l;
{
    if (tlist(l) == lcons) {
	printf("L%s",SP);
	(*fun)(lhd(l));
	plist(fun, ltl(l));
    } else
	printf("N%s",SP);
}

void
pid(i)
id i;
{
    printf("#%s\t", i);
}

#if defined(sequent) || defined(vax)
vfprintf(f, fmt, va)
FILE *f;
char *fmt;
int *va;
{
    return fprintf(f, fmt, va[0], va[1], va[2], va[3]);
}

char *
vsprintf(s, fmt, va)
char *s;
char *fmt;
int *va;
{
    return sprintf(s, fmt, va[0], va[1], va[2], va[3]);
}
#endif

/*VARARGS0*/
void
error(va_alist)
va_dcl
{
    va_list args;
    char *fmt;

    va_start(args);
    fmt = va_arg(args, char *);
    fprintf(stderr, "%s: Error ", progname);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    exit(1);
}

/*VARARGS0*/
void
errmsg(va_alist)
va_dcl
{
    va_list args;
    char *fmt;
    extern int interactive;

    va_start(args);
    fmt = va_arg(args, char *);

    if (interactive) {
	char b[1024];
	vsprintf(b, fmt, args);
	picmdmsgnp(b);
    } else
	vfprintf(stderr, fmt, args);
    va_end(args);
}

/*VARARGS0*/
void
normmsg(va_alist)
va_dcl
{
    va_list args;
    char *fmt;
    extern int interactive;

    va_start(args);
    fmt = va_arg(args, char *);

    if (interactive) {
	char b[1024];
	vsprintf(b, fmt, args);
	picmdmsgnp(b);
    } else
	vfprintf(stderr, fmt, args);
    va_end(args);
}

char *
mtupstr(l, n)
list l;
int n;
{
    if (tlist(l) == lcons)
	return mtupstr(ltl(l), n+1);
    else {
	char *p = malloc(10);
	sprintf(p, "P#%d", n);
	return p;
    }
}

tree
remaketuple(l)
list l;
{
    tree f = mkident(mtupstr(l, 0));

    while(tlist(l) != lnil) {
	f = mkap(f, lhd(l));
	l = ltl(l);
    }
    return f;
}

void
pasym(i)
sym i;
{
    switch(tsym(i)) {
    case gramterm: 
	printf("M%s%c%s",SP,gcharacter(i),SP);
	break;
    case gramintterm: 
	printf("L%s#%lu\t", SP, (long)gcint(i));
	break;
    case gramidterm:
	printf("R%s#%s\t" , SP, gcid(i));
	break;
    case gramsymterm:
	printf("N%s#%s\t" , SP, gcsym(i)); 
	break;                
    case expterm: 
	printf("U%s%c%s",SP,gcharacter2(i),SP);
	break;
    case expintterm:
	printf("D%s#%lu\t", SP, (long)gcint2(i));
	break;
    case expidterm:
	printf("G%s#%s\t" , SP, gcid2(i));
	break;
    case expsymterm:
	printf("B%s#%s\t" , SP, gcsym2(i));
	break;
    case antiquote:
	printf("q%s",SP);
	ptree(gexpr(i));
	break;
    case nonterminal:
	printf("n%s",SP);
	pttype(gtype(i));
	break;
    case list1: 
	printf("&%s",SP);
	pttype(gtypel1(i));
	plist(pasym,gterms1(i));
	break;
    case list0: 
	printf("%%%s",SP);
	pttype(gtypel0(i));
	plist(pasym,gterms0(i));
	break;
    default:
	error("Bad pasym");
	break;
    }
}

void
paassoc(a)
assoc a;
{
        switch(tassoc(a)) {
        case leftassoc  : 
                          printf("L%s",SP);    
                          break;
        case rightassoc  : 
                          printf("R%s",SP);    
                          break;
        case nonassoc  : 
                          printf("N%s",SP);    
                          break;
        case bothassoc  : 
                          printf("B%s",SP);    
                          break;
       	}
}

void
pabrack(b)
brack b;
{ 
	switch(tbrack(b)) {
	case normal     :
			  printf("D%s",SP);
			  plist(pasym,gconstrn(b));
     	                  printf("#%u\t", gprecn(b));
                          paassoc(gassn(b));
			  break;
	case forget     :
			  printf("G%s",SP);
			  plist(pasym,gconstrf(b));
     	                  printf("#%u\t", gprecf(b));
                          paassoc(gassf(b));
			  break;
        default:
			  error("Bad brack");
			  break;
	}
}

void
pgepairs(l)
list l;
{
    if (tlist(l) == lcons) {
	ptree((tree)lhd(l));
	ptree((tree)ltl(l));
    } else
	error("Bad pgepairs");
}

void
prstmt(t)
stmt t;
{
 again:
    switch(tstmt(t)) {
    case stmtexp:
	printf("e%s", SP);
	ptree(gseexp(t));
	break;
    case stmtexpstmt:
	printf("s%s", SP);
	ptree(gsesexp(t));
	t = gsesstmt(t);
	goto again;
    case stmtlet:
	printf("l%s", SP);
	prbind(gslbind(t));
	t = gslstmt(t);
	goto again;
    case stmtbind:
	printf("b%s", SP);
	ptree(gsbpat(t));
	ptree(gsbexp(t));
	t = gsbstmt(t);
	goto again;
    default:
	error("Bad stmt");
    }
}

static void
pefield(p)
pair p;
{
    pid(pfst(p));
    ptree(psnd(p));
}

#define pint(i)	printf("#%d\t", (i));
static void pintf(i) { printf("#%d\t", (i)); }
/* 	Performs a pre order walk of the tree
**	to print it.
*/
void
ptree(t)
tree t;
{
 again:
    switch(ttree(t)) {
    case par:
	t = gpare(t);
	goto again;
    case hmodule:
	printf("h%s",SP);
	pid(ghmodid(t));
	pimpstuff((impstuff)ghexp(t), prexpid);
	plist(pimpstuff, ghimp(t));
	plist(pimpstuff, ghfix(t));
	prbind((binding)ghbind(t));
	break;
    case module:
	printf("m%s",SP);
	pfixes();
	plist(pimpid, gimplist(t));
	if (gexplist(t)) {
	    printf("=%s",SP);
	    plist(pid, gexplist(t));
	} else {
	    printf("`%s",SP);
	}
	prbind((binding)gmodlist(t));
	break;
    case ident: 
	printf("i%s",SP); 
	pid(gident(t));
	break;
    case integer:
	printf("I%s#%lu\t", SP, (long)ginteger(t));
	break;
    case bignum:
	printf("J%s#%s\t", SP, gbignum(t));
	break;
    case ratnum:
	printf("Q%s#%s\t", SP, gratnum(t));
	break;
    case charr:
    {
	unsigned char *s = (unsigned char *)gchar(t);
	int c;
	if (s[0] == '\\' && s[1] == 'u')
	    sscanf(s+2, "%x", &c);
	else
	    c = *s;
	    printf("C%s#%d\t", SP, c);
	    break;
    }
    case floatt:
	printf("F%s#%24.18e\t", SP, gfloat(t));
	break;
    case string:
	printf("S%s#%s\t", SP, gstring(t));
	break;
    case ap: 
	printf("a%s",SP); 
	ptree(gfun(t)); 
	ptree(garg(t)); 
	break;
    case lam: 
	printf("l%s",SP);
	ptree(glamid(t));
	ptree(glamexpr(t));
	break;
    case cexpr:                             /* for conctypes */
	printf("E%s",SP);
	plist(pasym, gsymlist(t));
	break;
    case letv: 
	printf("e%s",SP);
	prbind((binding)gletvdeflist(t));
	ptree(gletvexpr(t));
	break;
    case casee:
	printf("c%s",SP);
	ptree(gcaseexpr(t));
	plist(ppbinding, gcasebody(t));
	break;
    case tuple:
	ptree(remaketuple(gtuplelist(t)));
	break;
    case as:
	printf("s%s",SP);
	pid(gasid(t));
	ptree(gase(t));
	break;
    case condp:
	printf("o%s",SP);
	ptree(gcondpp(t));
	ptree(gcondpe(t));
	break;
    case lazyp:
	printf("Z%s",SP);
	ptree(glazyp(t));
	break;
    case restr:
	printf("R%s",SP);
	ptree(grestre(t));
	pttype(grestrt(t));
	break;
    case eannot:
	printf("|%s",SP);
	ptree(geannote(t));
	pid(geannota(t));
	break;
    case listf:
	printf("1%s#%d\t", SP, glistt(t));
	plist(ptree, glistf(t));
	break;
    case listg:
	printf("2%s",SP);
	ptree(glgg(t));
	plist(pqual, glgq(t));
	break;
    case wherev:
	printf("W%s",SP);
	plist(pgepairs, gwges(t));
	prbind((binding)gwdefs(t));
	break;
    case doexp:
	printf("D%s", SP);
	prstmt(gstmt(t));
	break;
    case record:
	printf("r%s", SP);
	ptree(grecid(t));
	plist(pefield, grecfields(t));
	break;
    default:
	error("Bad ptree %d", ttree(t));
    }
}

void
pqual(q)
qual q;
{
    switch(tqual(q)) {
    case qgen:
	printf("g%s",SP);
	ptree(gqgenpat(q));
	ptree(gqgenexp(q));
	break;
    case qfilter:
	printf("f%s",SP);
	ptree(gqfilter(q));
	break;
    case qlet:
	printf("l%s",SP);
	prbind(gqbinding(q));
	break;
    default:
	error("Bad pqual");
	break;
    }
}

void
ppragt(l)
list l;
{
    ttype typ = (ttype)lhd(l);
    finfot pragmas = (finfot)ltl(l);
    pttype(typ);
    if (pragmas)
	pfinfo(pragmas);
}

void
ppragtv(l)
list l;
{
    ttype typ = (ttype)lhd(l);
    id oi = (id)ltl(l);
    pttype(typ);
    if (oi)
	pid(oi);
}

void
prbind(b)
binding b;
{
    switch(tbinding(b)) {
    case tbind: 
	printf("t%s",SP);
	pttype(gtbindid(b));
	plist(patype, gtbindc(b));
	pimpstuff((impstuff)gtbindd(b), pid);
	break;
    case xbind: 
	printf("x%s",SP);
	pttype(gxbindid(b));
	plist(patype, gxbindc(b));
	pimpstuff((impstuff)gxbindd(b), pid);
	break;
    case pbind: 
	printf("p%s",SP);
	plist(ppbinding, gpbindl(b));
	break;
    case gbind: 				/* for conctypes */
	printf("b%s",SP);
	pttype(gtcbindid(b));
	plist(pabrack, gtcbindc(b));
	break;
    case abind: 
	printf("A%s",SP);
	prbind(gabindfst(b));
	prbind(gabindsnd(b));
	break;
    case rbind: 
	printf("r%s",SP);
	prbind(grbind(b));
	break;
    case lbind: 
	printf("O%s",SP);
	prbind(glbindfst(b));
	prbind(glbindsnd(b));
	break;
    case ebind:
	printf("z%s",SP);
	pttype(gebindid(b));
	pttype(gebindt(b));
	break;
    case cbind:
	printf(":%s",SP);
	pttype(gcbindt(b));
	prbind(gcbindb(b));
	break;
    case nbind:
	printf(";%s",SP);
	break;
    case ubind:
	printf("'%s",SP);
	plist(pttype, guids(b));
	break;
    case ibind:
	printf(".%s",SP);
	pttype(gitype(b));
	prbind(gibindb(b));
	break;
    case sbind:
	printf("+%s",SP);
	plist(pid, gbsids(b));
	pttype(gbstype(b));
	break;
    case specbind: 
	printf("<%s", SP);
	pid(gspecid(b));
	plist(ppragtv, gspectypes(b));
	break;
    case specinstbind:
	printf(">%s", SP);
	pttype(gspecinst(b));
	break;
    case vbind: 
	printf("v%s",SP);
	pttype(gvbindid(b));
	pttype(gvbindt(b));
	plist(patype, gvbindc(b));
        prbind(gvbindb(b));
	break;
    default:
	error("Bad prbind");
	break;
    }
}

static void
patypepair(p)
pair(p);
{
    plist(pid, pfst(p));
    pttype(psnd(p));
}

static void
pkind(t)
kind t;
{
    switch(tkind(t)) {
    case karrow:
	printf(">%s", SP);
	pkind(kfun(t));
	pkind(karg(t));
	break;
    case kground:
	printf("*%s",SP);
	break;
    default:
	fprintf(stderr, "bad kind\n");
	exit(1);
    }
}

void
pttype(t)
ttype t;
{
    switch (tttype(t)) {
    case tname: 
	printf("T%s",SP);
	printf("#%s\t", gtypeid(t));
	plist(pttype, gtypel(t));
	break;
    case tnamek:
	printf("K%s",SP);
	printf("#%s\t", gtypeidk(t));
	pkind(gkindk(t));
	plist(pttype, gtypelk(t));
	break;
    case tap: 
	printf("A%s",SP);
	pttype(gtapvar(t));
	plist(pttype, gtaptypel(t));
	break;
    case tvar: printf("y%s#%d\t", SP, gtvar(t));
	break;
    case tstrict: printf("!%s",SP);
	pttype(gtstrict(t));
	break;
    case tcontext: printf("C%s",SP);
	plist(pttype, gcontexts(t));
	pttype(gctype(t));
	break;
    case tsels: printf("S%s",SP);
	plist(patypepair, gselidstys(t));
	break;
    default:
	error("bad pttype");
	break;
    }
}

void
patype(a)
atype a;
{
    switch (tatype(a)) {
    case atc: 
	printf("1%s",SP);
	if (gatcctx(a) != Lnil) { plist(pttype, gatcctx(a)); printf("%s",SP); }
	printf("#%s\t", gatcid(a));
	plist(pttype, gatctypel(a));
	break;
    default:
	error("Bad tag in atype %d\n", tatype(a));
    }
}

void
pimpstuff(p, f)
impstuff p;
void (*f)();
{
    switch(timpstuff(p)) {
    case import:
	printf("7%s",SP);
	pid(gimodid(p));
	plist(pimpid, giimps(p));
	if (curryflag) {
	    plist(pimpstuff, gifixes(p));
	} else {
	    /* Hack, hack !! BUG !!! interface != 0*/
	    if (interface)
		pfixes();
	    else
		plist(pimpstuff, gifixes(p));
	}
	plist(pimpid, gients(p));
	pimpstuff(gispec(p), pid);
	plist(pimpstuff, girename(p));
	pint(g3qual(p));
	plist(pid, g3as(p));
	break;
    case ispec:
	printf("8%s",SP);
	pint(giexpose(p));
	plist(prexpid, giids(p));
	break;
    case irename:
	printf("9%s",SP);
	pid(girensrc(p));
	pid(girendst(p));
	break;
    case ifix:
	printf("0%s",SP);
	plist(pid, gifixids(p));
	pint(gifixass(p));
	pint(gifixprec(p));
	break;
    case inone:
	printf("`%s",SP);
	break;
    case isome:
	printf("=%s",SP);
	plist(f, gisome(p));
	break;
    case interface:
	printf("^%s",SP);
	pid(giimodid(p));
	plist(pimpid, giiimps(p));
	plist(pimpstuff, giifixes(p));
	plist(pimpid, giients(p));
	break;
    case itypinfo:
	printf("<%s",SP);
	pint(gincon(p));
	pint(giflat(p));
	break;
    default:
	error("Bad pimpstuff");
	break;
    }
}

void
prexpid(e)
expidt e;
{
    switch(texpidt(e)) {
    case expid:
	printf("3%s",SP);
	pid(gexpid(e));
	break;
    case expdd:
	printf("5%s",SP);
	pid(gexpdd(e));
	break;
    case exppdd:
	printf("4%s",SP);
	pid(gexppdd(e));
	break;
    case expl:
	printf("6%s",SP);
	pid(gexplid(e));
	plist(pid, gexpll(e));
	break;
    default:
	error("Bad prexpid");
	break;
    }
}

void
prinststr(p)
pair p;
{
    pid(pfst(p));
    pfinfo(psnd(p));
}

void
pimpid(i)
impidt i;
{
    switch (timpidt(i)) {
    case impid:
	printf("f%s",SP);
	pid(gimpid(i));
	pttype(gimptype(i));
	pfinfo(gimpfinfo(i));
	break;
    case imptype:
	printf("Y%s",SP);
	pttype(gimptypet(i));
	pimpstuff((impstuff)gimpder(i), pid);
	pimpstuff((impstuff)gimptypi(i), pid);
	break;
    case impeqtype:
	printf("@%s",SP);
	pttype(gimpeqtype(i));
	plist(patype, gimpeqcon(i));
	pimpstuff((impstuff)gimpeqder(i), pid);
	break;
    case impisotype:
	printf("/%s",SP);
	pttype(gimpisotype(i));
	plist(patype, gimpisocon(i));
	pimpstuff((impstuff)gimpisoder(i), pid);
	break;
    case impview:
	printf("W%s",SP);
	pttype(gimpviewtype(i));
	pttype(gimpviewof(i));
	plist(patype, gimpviewcon(i));
	break;
    case impimport:
	printf("{%s",SP);
	pid(gimpimpmodid(i));
	plist(prexpid, gimpimpexp(i));
	plist(pimpstuff, gimpimpren(i), pid);
	break;
    case impsyn:
	printf("}%s",SP);
	pttype(gimpsynsrc(i));
	pttype(gimpsyndst(i));
	break;
    case impclass:
	printf("[%s",SP);
	pttype(gimpclasst(i));
	prbind((binding)gimpclassd(i));
	plist(prinststr, gimpclasss(i));
	break;
    case impinst:
	printf("]%s",SP);
	pttype(gimpinstt(i));
	pint(gimpinstd(i));
        pid(gimpinstm(i));
	plist(prinststr, gimpinsts(i));
	break;
    case impids:
	if (tlist(gimpids(i)) == lcons &&
	    tlist(ltl(gimpids(i))) == lnil) {
	    printf("f%s",SP);
	    pid(lhd(gimpids(i)));
	    pttype(gimptypes(i));
	    pfinfo(gimpfinfos(i));
	} else {
	    printf("F%s",SP);
	    plist(pid, gimpids(i));
	    pttype(gimptypes(i));
	    pfinfo(gimpfinfos(i));
	}
	break;
    case impctype:
	printf("K%s",SP);
	pttype(gimpcttype(i));
	plist(pabrack, gimpctprod(i));
	break;
    default:
	error("Bad pimpid");
	break;
    }
}

void
pfixes()
{
    int m = nfixes(), i;
    int s;

    for(i = 0; i < m; i++) {
	s = fixtype(i);
	if (s >= 0) {
	    printf("L%s0%sL%s",SP,SP,SP);
	    pid(fixop(i));
	    printf("N%s",SP);
	    pint(s);
	    pint(9);
	}
    }
    printf("N%s",SP);
}

void
pstring(s)
char *s;
{
    printf("#%s\t", s);
}

void
pfinfo(f)
finfot f;
{
    switch(tfinfot(f)) {
    case nofinfo:
	break;
    case finfo: printf("*%s",SP);
	pid(fi1(f));
	pid(fi2(f));
	printf("#%d\t", fi3(f));
	break;
    case hfinfo:
	printf("_%s",SP);
	if (pfinline(f))
	    ptree((tree)pfinline(f));
	else
	    printf("_%s",SP);
	pstring(pfstrict(f));
	plist(pstring, pfentry(f));
	printf("#%d\t", pfarity(f));
	printf("#%d\t", pffrsize(f));
        if (pfinsts(f))
            plist(ppragt, pfinsts(f));
	else
	    plist(pttype, Lnil);
        pint(pfevaled(f));
	break;
    default:
	error("Bad pfinfo");
    }
}

void
ppbinding(p)
pbinding p;
{
    switch(tpbinding(p)) {
    case ppat:
	printf("d%s",SP);
	ptree(gppat(p));
	ptree(gpexpr(p));
	break;
    default:
	error("Bad pbinding");
	break;
    }
}

void
ploadpair(l)
list l;
{
    pid(lhd(l));
    plist(pimpid, ltl(l));
}    

void
pcloadpair(l)
pair l;
{
    pid(pfst(l));
    plist(pintf, psnd(l));
}    

void
picmd(p)
icmd p;
{
    switch(ticmd(p)) {
    case Iexpr:
	printf("{%s",SP);
	ptree(gIexpr(p));
	break;
    case Ibinding:
	printf("}%s",SP);
	prbind((binding)gIbinding(p));
	break;
    case Itload:
	printf("|%s",SP);
	plist(ploadpair, gItloads(p));
	break;
    case Icload:
	printf("c%s",SP);
	plist(pcloadpair, gIcloadnames(p));
	pimpstuff((impstuff)gIcloadimp(p), pid);
	break;
    case Imload:
	printf("@%s",SP);
	pid(gImloadfile(p));
	ptree(gImloadtree(p));
	break;
    case Imsg:
	printf("`%s",SP);
	ptree(gImsg(p));
	break;
    case Iwhatis:
	printf("?%s",SP);
	pid(gIwhatid(p));
	break;
    case Ishow_:
	printf("_%s",SP);
	pid(gIshow_(p));
	break;
    case Inull:
	printf("^%s",SP);
	break;
    case Ihelp:
	printf("H%s",SP);
	break;
    case Imbind:
	printf("M%s",SP);
	ptree(gImbpat(p));
	ptree(gImbexp(p));
	break;
    default:
	error("Bad icmd");
    }
    fflush(stdout);
}

char *
uniqueid()
{
    static int idno = 1;	/* Next identifier number */
    static char buff[10];

    sprintf(buff, "$I%d", idno++);
    return buff;
}

void
secprompt()
{
    if (skipping)
	picmdmsgnp("ERROR ");
    else
	picmdmsgnp("# ");
}

char *
copystring(s)
char *s;
{
    char *p;

    p = malloc(strlen(s) + 1);
    strcpy(p, s);
    return p;
}

struct errinfo {
    struct errinfo *next;
    char *func;
    char *file;
    int lineno;
} *allerrinfo;

/* Add information to the mapping from entity names to filename,line-number */
void
adderrinfo(func)
char *func;
{
    struct errinfo *p;
    extern int yylineno;
    extern char *filename;

    if (!func)
	return;
    for(p = allerrinfo; p; p = p->next)
	if (strcmp(p->func, func) == 0) {
	    if (yylineno < p->lineno)
		p->lineno = yylineno;
	    return;
	}
    p = (struct errinfo *)malloc(sizeof(struct errinfo));
    p->func = func;
    p->file = filename;
    p->lineno = yylineno;
    p->next = allerrinfo;
    allerrinfo = p;
}

void
dumperrinfo()
{
    struct errinfo *p;

    for(p = allerrinfo; p; p = p->next) {
	printf("L%s",SP);
	pid(p->func);
	pid(p->file);
	pint(p->lineno-1);		/* Subtracting 1 often gives a more correct line number !?! */
    }
    printf("%sN",SP);
}


char *
leftmostid(t)
tree t;
{
    for(;;) {
	switch(ttree(t)) {
	case par:
	    t = gpare(t);
	    break;
	case ident:
	    return gident(t);
	case ap:
	    t = gfun(t);
	    break;
	case as:
	    return gasid(t);
	case condp:
	    t = gcondpp(t);
	    break;
	case eannot:
	    t = geannote(t);
	    break;
	default:
	    return 0;
	}
    }
}

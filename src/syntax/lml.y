%{
#include "include.h"
#include "listgen.h"

#include "proto.h"

static void chkcurid PROTO((char *s));
static void initcurid PROTO((char *s));

extern tree root;
extern impstuff iroot;
extern int skipping;

int curryflag = 0;

int prec = 0;                 /* conctype precedence */
assoc actass ;                /* conctype associativity */

#define UFRS (-2)
extern int zyzflag;
extern id installid();
extern tree niltree;
extern list Lnil;
#define lsing(l) mklcons(l, Lnil)
#define ldub(l1, l2) mklcons(l1, lsing(l2))
extern int Eflag;

extern list lconc(), lapp(), nrev();
extern tree mkbool(), mkbinop(), mkunop(), mkcons(), mkif(), mkand(), mkor(),
	    mknot(), mkfcomp(), mksection(), mkterop();
extern char *tupstr();
extern void pimpid();
extern char loadname[];
finfot finfofromstring();
char *leftmostid();

tree checkpat();

static id curid;
static int curarity, curfrsize, curevaled;
static list curentries;
static tree curinline;
static char *curstrict;
static list curinsts;
static binding buildrecord();

#define mkpair(x,y) mklcons(x,y)

%}

%union {
	tree utree;
	list ulist;
	ttype uttype;
	atype uatype;
	binding ubinding;
	pbinding upbinding;
	finfot ufinfo;
	impidt uimpid;
	id uid;
	long uuint;
	char *ustring;
	qual uqual;
	double ufloat;
	impstuff uimpstuff;
	char uterm;			/* for conctypes */
	sym usym;			/* for conctypes */
	brack ubrack;			/* for conctypes */
        assoc uassoc;                   /* for conctypes */
	pair upair;
}

%token 		NANNOT ENDANNOT
%token 		A_LINE A_INLINE A_ARITY A_STRICTNESS A_STRICT A_SHARED A_UNSHARED A_SPARK A_ENTRY A_FRAMESIZE A_NOEVAL A_OVERLOAD A_FLAT A_DERIVED A_FROMMODULE A_SPECIALIZE A_EVALED A_VECTORDEF A_VECREG2 A_LIMITOK A_METCALL A_NOTCHK A_EXPAND

%token		ID NONFIX INTCONST CHAR STRING FLOATCONST INTEGERCONST
		LAMBDA DOT LET LETREC IN WHERE WHEREREC LPAR RPAR 
		LBRACK RBRACK GUARD REC TYPE
		EQ NE LT GT LE GE
		IF THEN ELSE COMMA AND
		PLUS MINUS TIMES DIV MOD NOT ANDOP OROP COLON CONC INDEX
                FPLUS FMINUS FTIMES FDIV
    		IPLUS IMINUS ITIMES IDIV IMOD
    		LINDEX RAISE
		SEMI FCOMP EXCL
		MODULE END IMPORT EXPORT
		TFUN TPAIR
		CASE WILD AS LOCAL
%token		ASSIGN LBRACE RBRACE
%token		DCOLON
%token		SUCH LARROW DOTDOT
%token		ANNOT
%token		MINFIX MINFIXR MPREFIX MNONFIX MPOSTFIX
%token		SYNTAX_ERROR
%token		CONCTYPE LQUOTE RQUOTE           /* for conctypes < */
		TERMINAL INTTERMINAL IDTERMINAL SYMTERMINAL 
                ANTIQUOTE ANTIQVAR
		LFORGET RFORGET LNONTERM RNONTERM
		STARTCURLY DOTS ENDCURLYPLUS ENDCURLYMULT
                LEFTASSOC RIGHTASSOC NONASSOC  /* for conctypes > */
/* No new token definitions after this line!! */
%token		LOAD MLOAD OLOAD LEOF SOURCE WHATIS HELP SHOW_ THEEND


/*************************/
%nonassoc	LOW
%right		TFUN
%right		TPAIR
%nonassoc	EXCL
%nonassoc	LET LETREC IN REC TYPE
%nonassoc	WHERE WHEREREC
%right		AND
%nonassoc	LAMBDA 
%nonassoc	IF THEN ELSE
%right		SEMI ASSIGN
%nonassoc	AS
%right		COLON
%right		COMMA
%right		CONC
%right		DOT
%right		OROP
%right		ANDOP
%nonassoc	NOT
%right		EQ NE LT GT LE GE
%left 		PLUS MINUS FPLUS FMINUS IPLUS IMINUS
%left		TIMES DIV MOD FTIMES FDIV ITIMES IDIV IMOD
%right		RAISE
%left		INDEX LINDEX
%nonassoc	UMINUS
%left		FCOMP
%left		INFIXL
%right		INFIXR
%nonassoc	PREFIX POSTFIX
%nonassoc	ID NONFIX INTCONST NIL CHAR STRING WILD FLOATCONST INTEGERCONST
%right		LBRACK
%left		PREC_AP LPAR LBRACE CASE LOCAL LQUOTE   
%left		P_ANNOT

%type <upair> comp
%type <uimpstuff> typinfo
%type <utree> aexpr expr items cpexpr apexpr pexpr pitems module epat exp zexpr
%type <usym>  cexpr qpexpr                        /* conctypes */
%type <ulist> imports exports impids constrs expids cases typevars funs types stypes tpairs qual strings varids lfiles oloads oload comps
              cexprs cconstr cconstrs qpexprs terminals   /* conctypes */
%type <uqual> qual1
%type <uimpid> impid
%type <ubinding> binding
%type <ustring> STRING INTEGERCONST OLOAD MLOAD
%type <ufloat> FLOATCONST
%type <uid> iid ID NONFIX ident INFIXL INFIXR PREFIX POSTFIX fixid id binop WILD tid ttid binop1 op annot ANNOT TFUN CHAR
            ANTIQVAR                       /* conctypes */
%type <uuint> INTCONST int
%type <uttype> type typename typevar atype stype
%type <ufinfo> finfo
%type <upbinding> casee fun
%type <uatype> constr
%type <uterm> TERMINAL                     /* conctypes */
%type <uuint> INTTERMINAL                   /* conctypes */
%type <uid> IDTERMINAL SYMTERMINAL         /* conctypes */
%type <usym> symbol terminal		   /* conctypes */
%type <ubrack> bracket			   /* conctypes */
%type <upair> precinfo                     /* conctypes */
%type <uassoc> ass                         /* conctype */
%type <uid> EQ NE LT GT LE GE TIMES DIV MOD PLUS MINUS FTIMES FDIV FPLUS FMINUS ITIMES IDIV IMOD IPLUS IMINUS INDEX LINDEX RAISE DOT CONC FCOMP COMMA NOT OROP ANDOP
%type <uid> conid varid avarid
%type <uid> epragma

%%
top     : 'i' commands end | unit | 'p' interf;

/** Interactive **/

end	: THEEND { seteof(); }

commands: commands command
          |
          /* empty */
          ;

command	: expr SEMI                     { picmd(mkIexpr($1)); }
          |
          LET binding SEMI              { picmd(mkIbinding($2)); }
          |
	  LOAD lfiles SEMI { switchto(nrev(&$2)); } load
          |
	  SOURCE STRING SEMI		{ source($2); }
	  |
	  LEOF				{ picmd(mkInull()); }
	  |
	  infix                         { picmd(mkInull()); }
	  |
	  WHATIS iid SEMI		{ picmd(mkIwhatis($2)); }
	  |
	  SHOW_ iid SEMI		{ picmd(mkIshow_($2)); }
	  |
	  HELP SEMI			{ picmd(mkIhelp()); }
	  |
          SEMI				{ picmd(mkInull()); }
          |
          error { skipping++; } SEMI	{ picmd(mkInull()); skipping = 0; }
          ;

/* identical to binding except for funs, where SEMI is not allowed anymore */
lfiles	: STRING			{ $$ = lsing($1); }
	  |
	  lfiles lfdelim STRING		{ $$ = mklcons($3, $1); }
	  ;
lfdelim	: COMMA | /* empty */ ;

load	: oloads LEOF			{ picmd(mkItload($1)); }
	  |
	  MLOAD module LEOF 		{ picmd(mkImload($1,$2)); }
	  |
    	  LEOF				{ picmd(mkInull()); }
	  ;
oloads	: oload				{ $$ = lsing($1); }
	  |
	  oload oloads 			{ $$ = mklcons($1, $2); }
	  ;
oload	: OLOAD imports			{ $$ = mkpair($1, $2); }
	  ;

/** Interface **/
interf	: imports		{ iroot = mkinterface("_lml", Lnil, Lnil, $1); /*!!!*/}
	;

/** Compiler **/

unit	: imports expr		{ root = Eflag ? $2 :
				    mkmodule(
					$1,
					lsing("Pmain"),
					    mkpbind(lsing(mkppat(
					       mkident("Pmain"), 
					       mklam(mkident("_input"), $2))
						    )));
					} |
          module		{ root = $1; }
          ;

module	: MODULE imports exports binding END
					{ $$ = mkmodule($2, $3, $4); }
	  ;


infix	: MINFIX STRING SEMI		{ makeinfix($2, INFIXL); }
	  |
	  MINFIXR STRING SEMI		{ makeinfix($2, INFIXR); }
	  |
	  MPREFIX STRING SEMI		{ makeinfix($2, PREFIX); }
	  |
	  MPOSTFIX STRING SEMI		{ makeinfix($2, POSTFIX); }
	  |
	  MNONFIX STRING SEMI		{ makeinfix($2, NONFIX); }
	  ;
  
imports	: imports IMPORT impids SEMI	{ $$ = lconc($1, $3); } |
          imports infix                 { $$ = $1; } |
	  /* empty */			{ $$ = Lnil; }
	  ;

impids	: impid				{ $$ = lsing($1); } |
	  impids COMMA impid	 	{ $$ = lapp($1, $3); }
	  ;

impid	: iid { initcurid($1); } COLON type finfo		{ $$ = mkimpid($1, $4, $5); } |
	  TYPE typename	typinfo		{ $$ = mkimptype($2, mkinone(), $3); } |
	  TYPE typename EQ constrs	{ $$ = mkimpeqtype($2, $4, mkinone()); } |
          TYPE typename EQ EQ type      { $$ = mkimpsyn($2, $5); } |
          CONCTYPE typename EQ cconstrs { prec =0; 
                                          $$ = mkimpctype($2, $4); }  /*conctype*/
	  ;

typinfo	: /* empty */			{ $$ = mkinone(); } |
          A_FLAT INTCONST ENDANNOT		{ $$ = mkitypinfo($2, 1); }
	  ;

exports	: EXPORT expids SEMI		{ $$ = $2; } |
	  EXPORT SEMI			{ $$ = Lnil; } |
	  /* empty */			{ $$ = 0; }
	  ;

expids	: iid				{ $$ = lsing($1); } |
	  expids COMMA iid 		{ $$ = lapp($1, $3); }
	  ;

iid	: fixid				{ $$ = $1; } |
	  id				{ $$ = $1; } |
          LPAR ident RPAR		{ $$ = $2; }
	  ;

fixid	: INFIXL			{ $$ = $1; } |
	  INFIXR			{ $$ = $1; } |
	  PREFIX			{ $$ = $1; } |
	  POSTFIX			{ $$ = $1; }
	  ;


aexpr	: id				{ $$ = mkident($1); } |
	  INTCONST			{ $$ = mkinteger($1); } |
          FLOATCONST			{ $$ = mkfloatt($1); } |
          INTEGERCONST			{ $$ = mkbignum($1); } |
/*          MINUS FLOATCONST		{ $$ = mkfloatt(-$2); } | causes many conflicts */
	  CHAR				{ $$ = mkcharr($1); } |
	  STRING			{ $$ = mkstring($1); } |
	  LPAR zexpr RPAR		{ $$ = mkpar($2); } |
	  LBRACK items RBRACK		{ $$ = $2; } |
	  LBRACK RBRACK			{ $$ = niltree; } |
	  LBRACK expr DOTDOT RBRACK	{ $$ = mklistf(L_FROM, lsing($2)); } |
	  LBRACK expr SEMI expr DOTDOT RBRACK	{ $$ = mklistf(L_FROM_BY, ldub($2, $4)); } |
	  LBRACK expr DOTDOT expr RBRACK	{ $$ = mklistf(L_FROM_TO, ldub($2, $4)); } |
	  LBRACK expr SEMI expr DOTDOT expr RBRACK	{ $$ = mklistf(L_FROM_BY_TO, mklcons($2, ldub($4, $6))); } |
	  LBRACK expr SUCH qual RBRACK	{ $$ = mklistg($2, nrev(&$4)); } |
	  LPAR expr binop RPAR		{ $$ = mkap(mkident($3), $2); } |
	  LPAR binop1 expr RPAR		{ $$ = mksection($2, $3); } |
	  CASE expr IN cases END	{ $$ = mkcasee($2, $4); } |
	  LQUOTE cexprs RQUOTE		{ $$ = mkcexpr($2) ; }  /* conctypes */
	  ;

cexprs  : /* empty */                   { $$ = Lnil;}     | /* conctypes < */
          cexprs cexpr                  { $$ = lapp($1,$2) ; }
	  ;
cexpr   : TERMINAL                      { $$ = mkexpterm($1) ; } |
          INTTERMINAL                   { $$ = mkexpintterm($1) ; } | 
          IDTERMINAL                    { $$ = mkexpidterm($1) ; } |
          SYMTERMINAL                   { $$ = mkexpsymterm($1) ; } |
          ANTIQUOTE LPAR expr RPAR      { $$ = mkantiquote($3) ; } |
	  ANTIQVAR  			{ $$ = mkantiquote(mkident($1)) ;}
	  ;                                               /* conctypes > */

zexpr   : expr %prec LOW		{ $$ = $1; }
	  |
	  expr SEMI zexpr		{ $$ = mkbinop("_THEN", $1, $3); }
	  |
	  aexpr ASSIGN expr SEMI zexpr { $$ = mkbinop("_BIND", $3, mklam(checkpat($1), $5)); }
	  ;

expr	: aexpr				{ $$ = $1; } |
	  expr COLON type		{ $$ = mkrestr($1, $3); } |
	  LET EXCL cpexpr EQ expr IN expr { $$ = mkcasee($5,lsing(mkppat($3,$7))); } |
	  LET binding IN expr 		{ $$ = mkletv($2,$4); } |
	  expr WHERE binding 		{ $$ = mkletv($3,$1); } |
	  LAMBDA apexpr DOT expr %prec LAMBDA	{ $$ = mklam($2,$4); } |
	  IF expr THEN zexpr ELSE expr	{ $$ = mkterop("Pif", $2, $4, $6); } |
	  expr OROP expr 		{ $$ = mkbinop($2, $1,$3); } |
	  expr ANDOP expr 		{ $$ = mkbinop($2, $1,$3); } |
	  NOT expr 			{ $$ = mkunop($1, $2); } |
	  expr EQ expr 			{ $$ = mkbinop($2, $1,$3); } |
	  expr NE expr 			{ $$ = mkbinop($2, $1,$3); } |
	  expr LT expr			{ $$ = mkbinop($2, $1,$3); } |
	  expr GT expr			{ $$ = mkbinop($2, $1,$3); } |
	  expr LE expr			{ $$ = mkbinop($2, $1,$3); } |
	  expr GE expr			{ $$ = mkbinop($2, $1,$3); } |
	  expr TIMES expr		{ $$ = mkbinop($2, $1,$3); } |
	  expr DIV expr			{ $$ = mkbinop($2, $1,$3); } |
	  expr MOD expr			{ $$ = mkbinop($2, $1,$3); } |
	  expr PLUS expr		{ $$ = mkbinop($2, $1,$3); } |
	  expr MINUS expr 		{ $$ = mkbinop($2, $1, $3); } |
	  expr FTIMES expr		{ $$ = mkbinop($2, $1,$3); } |
	  expr FDIV expr		{ $$ = mkbinop($2, $1,$3); } |
	  expr FPLUS expr		{ $$ = mkbinop($2, $1,$3); } |
	  expr FMINUS expr 		{ $$ = mkbinop($2, $1,$3); } |
	  expr ITIMES expr		{ $$ = mkbinop($2, $1,$3); } |
	  expr IDIV expr		{ $$ = mkbinop($2, $1,$3); } |
	  expr IMOD expr		{ $$ = mkbinop($2, $1,$3); } |
	  expr IPLUS expr		{ $$ = mkbinop($2, $1,$3); } |
	  expr IMINUS expr 		{ $$ = mkbinop($2, $1,$3); } |
	  expr INDEX expr 		{ $$ = mkbinop($2, $1, $3); } |
	  expr LINDEX expr 		{ $$ = mkbinop($2, $1, $3); } |
	  expr RAISE expr 		{ $$ = mkbinop($2, $1, $3); } |
	  expr INFIXL expr		{ $$ = mkbinop($2, $1, $3); } |
	  expr INFIXR expr		{ $$ = mkbinop($2, $1, $3); } |
	  MINUS expr %prec UMINUS 	{ $$ = mkunop("_negate", $2); } |
          FMINUS expr %prec UMINUS      { $$ = mkunop("PFloatNeg", $2); } |
          IMINUS expr %prec UMINUS      { $$ = mkunop("PIntegerNeg", $2); } |
	  expr aexpr %prec PREC_AP	{ $$ = mkap($1,$2); } |
	  expr DOT  expr		{ $$ = mkcons($1, $3); } |
	  expr CONC expr		{ $$ = mkbinop($2, $1,$3); } |
	  expr FCOMP expr		{ $$ = mkfcomp($1, $3); } |
	  PREFIX expr			{ $$ = mkunop($1, $2); } |
	  expr POSTFIX			{ $$ = mkunop($2, $1); } |
	  expr COMMA expr		{ if (ttree($3) == tuple)
						$$ = mktuple(mklcons($1, gtuplelist($3)));
					  else
						$$ = mktuple(ldub($1, $3));
					} |
	  aexpr annot %prec P_ANNOT	{ $$ = mkeannot($1, $2); }
	  ;

qual	: qual SEMI qual1		{ $$ = mklcons($3, $1); } |
	  qual1				{ $$ = lsing($1); }
	  ;

qual1	: epat LARROW expr		{ $$ = mkqgen($1, $3); } |
	  expr				{ $$ = mkqfilter($1); }
	  ;
epat	: expr				{ $$ = checkpat($1); }
	  ;



id	: LPAR op RPAR			{ $$ = $2; } |
	  ident				{ $$ = $1; }
	  ;

op	: binop				{ $$ = $1; } |
	  NOT				{ $$ = $1; } |
	  PREFIX			{ $$ = $1; } |
	  POSTFIX			{ $$ = $1; }
	  ;

binop	: binop1			{ $$ = $1; } |
	  MINUS 			{ $$ = $1; }
	  ;

binop1	: OROP 				{ $$ = $1; } |
	  ANDOP 			{ $$ = $1; } |
	  EQ 				{ $$ = $1; } |
	  NE 				{ $$ = $1; } |
	  LT				{ $$ = $1; } |
	  GT				{ $$ = $1; } |
	  LE				{ $$ = $1; } |
	  GE				{ $$ = $1; } |
	  TIMES				{ $$ = $1; } |
	  DIV				{ $$ = $1; } |
	  MOD				{ $$ = $1; } |
	  PLUS				{ $$ = $1; } |
	  INFIXL			{ $$ = $1; } |
	  INFIXR			{ $$ = $1; } |
	  DOT 				{ $$ = $1; } |
	  CONC				{ $$ = $1; } |
	  FCOMP				{ $$ = $1; } |
	  COMMA				{ $$ = $1; } |
	  INDEX				{ $$ = $1; } |
	  LINDEX			{ $$ = $1; } |
	  RAISE				{ $$ = $1; } |
          FPLUS				{ $$ = $1; } |
          FMINUS			{ $$ = $1; } |
          FTIMES			{ $$ = $1; } |
          FDIV				{ $$ = $1; } |
          IPLUS				{ $$ = $1; } |
          IMINUS			{ $$ = $1; } |
          ITIMES			{ $$ = $1; } |
          IDIV				{ $$ = $1; } |
          IMOD				{ $$ = $1; }
	  ;

items	: expr SEMI items		{ $$ = mkcons($1, $3); } |
	  expr				{ $$ = mkcons($1, niltree); }
	  ;

cases	: casee				{ $$ = lsing($1); } |
	  cases GUARD casee		{ $$ = lapp($1, $3); }
	  ;

casee	: cpexpr COLON zexpr		{ $$ = mkppat($1, $3); }
	  ;

binding:  funs				{ $$ = mkpbind($1); } |
	  TYPE typename EQ LBRACE comps RBRACE { $$ = buildrecord($2, nrev(&$5)); } |
	  TYPE typename EQ constrs	{ $$ = mktbind($2, $4, mkinone()); adderrinfo(gtypeid($2)); } |
          TYPE typename EQ EQ type      { $$ = mkebind($2, $5); } |
          id DCOLON type		{ $$ = mksbind(lsing($1), $3); } |
	  CONCTYPE typename EQ cconstrs { $$ = mkgbind($2, $4); } | /*conctype*/
	  binding AND binding		{ $$ = mkabind($1, $3); } |
	  REC binding			{ $$ = mkrbind($2); } |
	  LPAR binding RPAR		{ $$ = $2; } |
	  LOCAL binding IN binding END	{ $$ = mklbind($2, $4); }
	  ;

typename: tid				{ $$ = mktname($1, Lnil); } |
	  tid typevars			{ $$ = mktname($1, $2); } |
	  LPAR typename RPAR		{ $$ = $2; }
	  ;

typevars: typevar typevars		{ $$ = mklcons($1, $2); } |
	  typevar			{ $$ = lsing($1); }
	  ;

funs	: fun 				{ $$ = lsing($1); } |
	  funs GUARD fun		{ $$ = lapp($1, $3); }
	  ;

fun	: cpexpr EQ expr %prec LET	{ $$ = mkppat($1, $3); adderrinfo(leftmostid($1)); }
	  ;

apexpr	: id				{ $$ = mkident($1); } |
	  WILD				{ $$ = mkident("_"); } |
	  INTCONST			{ $$ = mkinteger($1); } |
	  MINUS INTCONST		{ $$ = mkinteger(-$2); } |
	  CHAR				{ $$ = mkcharr($1); } |
	  STRING			{ $$ = mkstring($1); } |
	  LPAR pexpr RPAR		{ $$ = mkpar($2); }	|
	  LBRACK pitems RBRACK		{ $$ = $2; } |
	  LBRACK RBRACK			{ $$ = niltree; } |
	  LQUOTE qpexprs RQUOTE		{ $$ = mkcexpr($2);} /* conctypes */
	  ;

qpexprs : /* empty */                   { $$ = Lnil;}      | /* conctypes <  */
	  qpexprs qpexpr                { $$ = lapp($1,$2) ; } 
	  ;

qpexpr  : TERMINAL                      { $$ = mkexpterm($1) ; } |
          INTTERMINAL                   { $$ = mkexpintterm($1) ; } | 
          IDTERMINAL                    { $$ = mkexpidterm($1) ; } |
          SYMTERMINAL                   { $$ = mkexpsymterm($1) ; } |
          ANTIQUOTE LPAR pexpr RPAR     { $$ = mkantiquote($3) ; } |
	  ANTIQVAR			{ $$ = mkantiquote(mkident($1)); }
	  ;                                             /* conctypes >  */

pexpr	: apexpr			{ $$ = $1; } |
	  pexpr COLON type		{ $$ = mkrestr($1, $3); } |
	  id AS pexpr			{ $$ = mkas($1, $3); } |
	  pexpr DOT pexpr		{ $$ = mkcons($1, $3); } |
	  pexpr pexpr %prec PREC_AP	{ $$ = mkap($1,$2); } |
	  pexpr INFIXL pexpr		{ $$ = mkbinop($2, $1, $3); } |
	  pexpr INFIXR pexpr		{ $$ = mkbinop($2, $1, $3); } |
	  PREFIX pexpr			{ $$ = mkunop($1, $2); } |
	  pexpr POSTFIX			{ $$ = mkunop($2, $1); } |
	  pexpr COMMA pexpr		{ if (ttree($3) == tuple)
						$$ = mktuple(mklcons($1, gtuplelist($3)));
					  else
						$$ = mktuple(ldub($1, $3));
					}
	  ;

cpexpr	: pexpr %prec PREC_AP		{ $$ = $1; } |
	  pexpr ANDOP LPAR expr RPAR	{ $$ = mkcondp($1, $4); }
	  ;

pitems	: pexpr SEMI pitems		{ $$ = mkcons($1, $3); } |
	  pexpr				{ $$ = mkcons($1, niltree); }
	  ;

atype	: ttid				{ $$ = mktname($1, Lnil); } |
          LBRACK type RBRACK		{ $$ = mktname("_List", lsing($2)); } |
	  LPAR type RPAR		{ $$ = $2; } |
	  typevar			{ $$ = $1; }
	  ;

type	: ttid types			{ $$ = mktname($1, $2); } |
	  atype				{ $$ = $1; } |
	  type TFUN type		{ $$ = mktname($2, ldub($1, $3)); } |
	  type INFIXL type		{ $$ = mktname($2, ldub($1, $3)); } |
	  type INFIXR type		{ $$ = mktname($2, ldub($1, $3)); } |
	  PREFIX type			{ $$ = mktname($1, lsing($2)); } |
	  type POSTFIX			{ $$ = mktname($2, lsing($1)); } |
	  atype TPAIR atype tpairs	{ $$ = mktname(installid(tupstr($4, 2)),
					               mklcons($1, mklcons($3, $4))); }
	  ;

tpairs	: /* */				{ $$ = Lnil; } |
	  TPAIR atype tpairs		{ $$ = mklcons($2, $3); }
	  ;

types	: types atype			{ $$ = lapp($1, $2); } |
	  atype				{ $$ = lsing($1); }
	  ;

typevar	: TIMES ID			{ $$ = mktvar(typeno($2)); }
	  ;

ttid	: LPAR TFUN RPAR		{ $$ = $2; } |
	  LPAR TPAIR RPAR		{ $$ = "_#2"; } |
	  tid				{ $$ = $1; }
	  ;

tid	: ident				{ $$ = $1; } |
	  LPAR fixid RPAR		{ $$ = $2; } |
	  LPAR ident RPAR		{ $$ = $2; }
	  ;

constrs	: constrs PLUS constr		{ $$ = lapp($1, $3); } |
	  constr			{ $$ = lsing($1); }
	  ;

constr	: tid stypes			{ $$ = mkatc($1, Lnil, $2); } |
	  stype INFIXL stype		{ $$ = mkatc($2, Lnil, ldub($1, $3)); } |
	  stype INFIXR stype		{ $$ = mkatc($2, Lnil, ldub($1, $3)); } |
	  PREFIX stype			{ $$ = mkatc($1, Lnil, lsing($2)); } |
	  stype POSTFIX			{ $$ = mkatc($2, Lnil, lsing($1)); } /*|
	  LPAR constr RPAR		{ $$ = $2; } */
	  ;

stypes	: /* empty */			{ $$ = Lnil; } |
	  stypes stype			{ $$ = lapp($1, $2); }
	  ;

stype	: atype				{ $$ = $1; } |
	  atype EXCL			{ $$ = mktstrict($1); }
	  ;

cconstrs: cconstrs PLUS bracket		{ $$ = lapp($1, $3); } | /*conctypes <*/ 
	  bracket    		        { $$ = lsing($1); }
	  ;

bracket	: precinfo LQUOTE cconstr RQUOTE   { $$ = mknormal($3,pfst($1),psnd($1)); } |
	  precinfo LFORGET cconstr RFORGET { $$ = mkforget($3,pfst($1),psnd($1)); }
	  ;

precinfo : /* empty */                { $$ = mkppair(0,mkbothassoc ()) ; } |
             LT ass                   { prec = prec +1;  
                                        actass = $2;  
                                        $$ = mkppair(prec,$2);} |
             EQ                       { $$ = mkppair(prec,actass); } |
             INTCONST ass             { $$ = mkppair($1,$2) ;}  /* imports */
             ;

ass     : /* empty */                   { $$ = mkbothassoc(); } |
          LEFTASSOC                     { $$ = mkleftassoc(); } |
          RIGHTASSOC                    { $$ = mkrightassoc(); } |
          NONASSOC                      { $$ = mknonassoc(); } 
          ;

cconstr : /* empty */                   { $$ = Lnil; }		|
	  cconstr symbol 		{ $$ = lapp($1, $2); }
	  ;

symbol  : terminal                      { $$ = $1 ;} | 
	  LNONTERM type RNONTERM        { $$ = mknonterminal($2) ; } |
	  STARTCURLY LNONTERM type RNONTERM terminals DOTS ENDCURLYPLUS { $$ = mklist1($3,$5) ; } |
	  STARTCURLY LNONTERM type RNONTERM terminals DOTS ENDCURLYMULT { $$ = mklist0($3,$5) ; }
          ;

terminal :TERMINAL                      { $$ = mkgramterm($1); } |
          INTTERMINAL                   { $$ = mkgramintterm($1) ; } | 
          IDTERMINAL                    { $$ = mkgramidterm($1) ; } |
          SYMTERMINAL                   { $$ = mkgramsymterm($1) ; } 
          ;

terminals: /* empty */                  { $$ = Lnil; } |
	  terminals terminal            { $$ = lapp($1,$2) ; } 
	  ;						  
                          /* conctypes > */


/*
finfo	: 			{ $$ = mknofinfo(); } |
	  LBRACE COMMA ID ENDANNOT	{ $$ = mkfinfo("_", $3, UFRS); } |
	  LBRACE ID COMMA ID ENDANNOT	{ $$ = mkfinfo($2, $4, UFRS); } |
	  LBRACE COMMA ID COMMA intconst ENDANNOT	{ $$ = mkfinfo("_", $3, $5); } |
	  LBRACE ID COMMA ID COMMA intconst ENDANNOT	{ $$ = mkfinfo($2, $4, $6); }
	  ;
intconst: INTCONST			{ $$ = $1; } |
	  MINUS INTCONST		{ $$ = -$2; }
	  ;

*/
finfo	: /* missing */			{ $$ = mknofinfo(); } |
	  ipragmas			{ $$ = mkhfinfo(curinline, curstrict, curentries, curarity, curfrsize, curinsts, curevaled); }
	  ;

ident	: ID				{ $$ = $1; } |
	  NONFIX			{ $$ = $1; }
	  ;

annot	: epragma			{ $$ = $1; }
	  ;

int:	INTCONST { $$ = $1; };
varid:	iid { $$ = $1; };
conid:	ident { $$ = $1; };
exp:	expr { $$ = $1; };

/*o_int:		int | *** empty */

varids:		/* empty */ { $$ = Lnil; } | varids varid { $$ = mklcons($2,$1); };

strings:	/* empty */ { $$ = Lnil; } | strings STRING { $$ = mklcons($2,$1); };

/*
aline:		A_LINE int o_int STRING ENDANNOT
                ;
*/

ipragmas:	/* empty */ | ipragmas ipragma;

ipragma:	A_INLINE avarid varids EQ exp ENDANNOT {
				tree et = $5;
				list vl = $3;
				chkcurid($2);
				while(tlist(vl) == lcons) {
				    et = mklam(mkident(lhd(vl)), et);
				    vl = ltl(vl);
				}
				curinline = et;
			}
		|
		A_STRICTNESS avarid EQ STRING conid ENDANNOT {
				char *con = $5;
				if (!zyzflag)
				    con++;
				chkcurid($2);
				if (strcmp(con, "ST") == 0)
				    curstrict = $4;
				else if (strcmp(con, "FIRST_ORDER") == 0) {
				    char *p = $4, buf[1000], *q = buf;
				    
				    while(*p) {
					if (*p == 'T') {
					    sprintf(q, "%d|", p - $4);
					    q += strlen(q);
					}
					p++;
				    }
				    if (q > buf)
					--q;
				    else
					*q++ = 'F';
				    strcat(q, ",F");
				    curstrict = installid(q);
				} else {
				    /* unknown strictness info */
				}
			}
		|
		A_ENTRY avarid EQ strings ENDANNOT {
				chkcurid($2);
				curentries = $4;
			}
		|
		A_ARITY avarid EQ int ENDANNOT {
				chkcurid($2);
				curarity = $4;
			}
		|
		A_FRAMESIZE avarid EQ int ENDANNOT {
				chkcurid($2);
				curfrsize = $4;
			}
                |
		A_EVALED avarid ENDANNOT {
				chkcurid($2);
				curevaled = 1;
			}
		;

avarid:		varid	{ $$ = $1; }
		|
		WILD	{ $$ = "_"; }
		;

epragma:	A_STRICT ENDANNOT { $$ = "STRICT"; }
		|
		A_SHARED ENDANNOT { $$ = ""; }
		|
		A_UNSHARED ENDANNOT { $$ = ""; }
		|
		A_SPARK varid ENDANNOT { char buf[1000]; sprintf(buf, "SPARK %s", $2); $$ = installid(buf); }
		|
		A_NOEVAL ENDANNOT { $$ = "NOEVAL"; }
		|
		A_VECTORDEF ENDANNOT { $$ = "VECTORDEF"; }
		|
		A_VECREG2 ENDANNOT { $$ = "VECREG2"; }
		|
		A_LIMITOK ENDANNOT { $$ = "LIMITOK"; }
		|
		A_METCALL ENDANNOT { $$ = "METCALL"; }
		|
		A_OVERLOAD ENDANNOT { $$ = "OVERLOAD"; }
		|
		A_NOTCHK ENDANNOT { $$ = "NOTCHK"; }
		|
		A_EXPAND ENDANNOT { $$ = "INLINE"; }
		;

comps	: comp				{ $$ = lsing($1); }
	| comps SEMI comp		{ $$ = mklcons($3, $1); }
	;

comp	: id COLON type			{ $$ = mkppair($1, $3); }
	;

%%

static list
mapl(f, l)
list (*f)();
list l;
{
    if (tlist(l) == lnil)
	return Lnil;
    else
	return mklcons((*f)(lhd(l)), mapl(f, ltl(l)));
}

void *fsnd(p) pair p; { return psnd(p); }
void *ffst(p) pair p; { return pfst(p); }

static binding
buildrecord(t, cs)
ttype t;
list cs;
{
    list types = mapl(fsnd, cs);
    char *tn = gtypeid(t);
    atype acon = mkatc(tn, Lnil, types);
    binding realtype = mktbind(t, lsing(acon), mkinone());
    list ids = mapl(ffst, cs);
    char buf[1000];
    list l;
    binding b;
    int k, len;

    for(len = 0, l = ids; tlist(l) != lnil; l = ltl(l), len++);
    for(k = 0, b = realtype, l = ids; tlist(l) != lnil; l = ltl(l), k++) {
	char *ii = (char *)lhd(l);
	tree dum = mkident("_");
	id sel, upd;
	sprintf(buf, "_.%s", ii+1);
	sel = installid(buf);
	makeinfix(sel+1, POSTFIX);
	sprintf(buf, "_.%s:=", ii+1);
	upd = installid(buf);
	makeinfix(upd+1, INFIXL);
	{
	    tree args;
	    int j;
	    tree var = mkident(ii);
	    for(args = mkident(tn), j = 0; j < len; j++) {
		args = mkap(args, j == k ? var : dum);
	    }
	    b = mkabind(b, mkpbind(lsing(mkppat(mkap(mkident(sel),
						     args),
						var))));
	}
	{
	    tree args, res;
	    int j;
	    list ll;
	    tree var = mkident(ii);
	    for(ll = ids, res = args = mkident(tn), j = 0; j < len; j++, ll = ltl(ll)) {
		args = mkap(args, j == k ? dum : mkident(lhd(ll)));
		res = mkap(res, mkident(lhd(ll)));
	    }
	    b = mkabind(b, mkpbind(lsing(mkppat(mkap(mkap(mkident(upd),
							  args),
						     var),
						res))));
	}
    }

    return mkrbind(b);
}

static void
chkcurid(s)
char *s;
{
    if (strcmp(s, curid) != 0 && strcmp(s, "_") != 0) {
	yyerror("Pragma not for current id");
    }
}


static void
initcurid(s)
char *s;
{
    curid = s;
    curarity = -1;
    curentries = Lnil;
    curinline = 0;
    curstrict = "";
    curfrsize = -1;
    curinsts = 0;
    curevaled = 0;
}

finfot
finfofromstring(s)
char *s;
{
    char args[100], res[10];
    int frs;

    args[0] = res[0] = '_';
    if (sscanf(s, ",%[TFU],%d", res+1, &frs) == 2) {
	return mkfinfo("_", installid(res), frs);
    } else if (sscanf(s, "%[TFU],%[TFU],%d", args+1, res+1, &frs) == 3) {
	return mkfinfo(installid(args), installid(res), frs);
    } else if (sscanf(s, ",%[TFU]", res+1) == 1) {
	return mkfinfo("_", installid(res), UFRS);
    } else if (sscanf(s, "%[TFU],%[TFU]", args+1, res+1) == 2) {
	return mkfinfo(installid(args), installid(res), UFRS);
    } else {
	return mknofinfo();
    }
}

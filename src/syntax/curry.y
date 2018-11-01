%{
#include "include.h"
#include "listgen.h"
extern int pedantic;
extern tree root;
extern impstuff iroot;
extern char empty_prel[];
extern char *incpath;

#include "proto.h"

extern char *filename;
extern int undefline;
extern int h1_3;
extern int skipping;
int curryflag = 1;
extern int zyzflag;

#define UFRS (-2)
extern id installid();
extern tree niltree;
extern id listid;
extern list Lnil;
extern int wantrcurl;
#define lsing(l) mklcons(l, Lnil)
#define ldub(l1, l2) mklcons(l1, lsing(l2))
extern int Eflag;

extern FILE *pathopen();

extern list lconc(), lapp(), nrev();
extern tree mkterop(), mkbinop(), mkunop(), mksection();
extern char *tupstr();
extern void pimpid();

static tree mkcons();
static ttype checkinst();
static char *typname();
extern char *leftmostid();
ttype checkclass();
void chkmodname();


static id curid;
/*
static int curarity, curfrsize, curevaled;
static list curentries;
static tree curinline;
static char *curstrict;
static list curinsts;
*/
static void chkcurid();
static finfot joinpragma();

id negbig();
list nrev();
tree mlist(), mkapchain(), mklams(), checkpat(), cndp(), checkexp();
binding andthem();
binding fbind();
list checkcontext();
finfot finfofromstring();
tree mkidentu();

static int ininterface = 0;

static int loadimport, qualload;

list autoloads;

int allowtypeexcl, allowas, allowstar;
extern int allowmulti;
static int allowasx;

%}

%union {
	tree utree;
	list ulist;
	pair upair;
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
	expidt uexpid;
	stmt ustmt;
	kind ukind;
}

%token CASE CLASS DATA DEFAULT DERIVING ELSE HIDING IF
%token IMPORT INFIX INFIXL INFIXR INSTANCE INTERFACE
%token MODULE OF RENAMING THEN TO TYPE WHERE LET IN
%token NEWTYPE DO QUALIFIED AS_KWD STAR VIEW
%token OLD_WHERE
%token NONFIX MNONFIX

%token LCURL RCURL SEMI COMMA LPAR RPAR AS ANNOT TILDE
%token WILD DCOLON ARROW LBRA RBRA BAR BACKSLASH DOTDOT DARROW
%token EQ LARROW MINUS
%token RATNUM CHAR INT STRING DIGIT INTCONST
%token CNAME NAME QNAME
%token TYPEEXCL

%token NANNOT ENDANNOT
%token A_LINE A_INLINE A_ARITY A_STRICTNESS A_STRICT A_SHARED A_UNSHARED A_SPARK A_ENTRY A_FRAMESIZE A_NOEVAL A_OVERLOAD A_FLAT A_DERIVED A_FROMMODULE A_SPECIALIZE A_EVALED A_NOTCHK

/* No new token definitions after the line below!! (If you don't know what you are doing.) */
%token ERREOF LEOF SYNTAX_ERROR LOAD MLOAD OLOAD SOURCE WHATIS HELP THEEND


/*******************************/
%nonassoc WHERE OLD_WHERE
%right ARROW
%nonassoc LET CASE PREC_LAM IF
%left DCOLON
%nonassoc OPN0
%left OPL0
%right OPR0
%nonassoc OPN1
%left OPL1
%right OPR1
%nonassoc OPN2
%left OPL2
%right OPR2
%nonassoc OPN3
%left OPL3
%right OPR3
%nonassoc OPN4
%left OPL4
%right OPR4
%nonassoc OPN5
%left OPL5
%right OPR5
%nonassoc OPN6
%left OPL6 MINUS UMINUS
%right OPR6
%nonassoc OPN7
%left OPL7
%right OPR7
%nonassoc OPN8
%left OPL8
%right OPR8
%nonassoc OPN9
%left OPL9
%right OPR9
%nonassoc CNAME NAME INT STRING CHAR RATNUM DIGIT WILD NONFIX INTCONST
%left PREC_AP LPAR LBRA TILDE
%left P_ANNOT
%nonassoc PREC10

%type <upair> istrinfo sel efield pfield
%type <utree> exp aexp pat apat epat o_gd gd xexp raexp rapat apat1
%type <ulist> ops texps tpats lpats imports exportlist context qual importelist qualn imports1 o_imports tyvars
%type <ulist> o_renaming renamings exports constrs atypes2 types apats alts alt altgdp1 altgdp gdp importes atypes1
%type <ulist> fungdp1 fungdp fgdp itopdecls iimports o_fixes itopdecl1s
%type <ulist> names names1 enames enames1 rnames1 satypes satypesl sels1 efields1 pfields1 pfields efields
%type <ulist> varids strings modids iprtypes iprtype iprtypevals iprtypeval istrinfos o_as varids1
%type <ufinfo> finfo ipragma ipragmas
%type <uimpstuff> impspec fix renaming import importr typinfo o_exportlist o_deriving limport
%type <uimpid> itopdecl iimport
%type <ubinding> topdecl decl topdecls decls o_where_class o_where_inst valdef clsign icdecl o_where_iclass icdecls funbind clsigns awhere let_left declcmd rtopdecl declcmds
%type <ustring> STRING ANNOT
%type <uid> op conid varid varop tycon NAME CNAME name MINUS modid ARROW iop INT DIGIT int ename binop1 NONFIX varcon RATNUM opx aid iinstmod what MLOAD OLOAD CHAR speccon tyconk oval
%type <uexpid> export importe
%type <uuint> o_digit INTCONST derinfo commas1 o_qualified
%type <uttype> type atype1 atype class_or_type con_simple simple class con_class con_inst inst context_or_type tyvar stype satype rtype xsatype
%type <uqual> qual1
%type <uatype> constr constr1
%type <ustmt> stmt
%type <ukind> kind
%type <ustring> OPN0 OPN1 OPN2 OPN3 OPN4 OPN5 OPN6 OPN7 OPN8 OPN9
%type <ustring> OPL0 OPL1 OPL2 OPL3 OPL4 OPL5 OPL6 OPL7 OPL8 OPL9
%type <ustring> OPR0 OPR1 OPR2 OPR3 OPR4 OPR5 OPR6 OPR7 OPR8 OPR9
%type <ustring> epragma

%%
top:	'i' commands end | module | 'p' sinterface;

end:	THEEND { seteof(); }

commands:commands command
         |
         /* empty */
         ;

command:exp SEMI				{ picmd(mkIexpr($1)); }
        |
        let_left SEMI				{ picmd(mkIbinding($1)); }
        |
	LOAD STRING SEMI 			{ switchto($2); loadimport = 0; autoloads = Lnil; } load
        |
	SOURCE STRING SEMI			{ source($2); }
	|
	LEOF					{ picmd(mkInull()); }
	|
        SEMI					{ picmd(mkInull()); }
        |
	WHATIS what SEMI			{ picmd(mkIwhatis($2)); }
	|
	declcmd SEMI				{ picmd(mkIbinding($1)); }
	|
	HELP SEMI				{ picmd(mkIhelp()); }
	|
	fix					{ picmd(mkInull()); }
	|
	LCURL declcmds rcurl SEMI		{ picmd(mkIbinding($2)); }
	|
	IMPORT o_qualified modids SEMI		{ switchtomod($3); loadimport = 1; qualload = $2; } load
	|
	xexp LARROW xexp SEMI			{ picmd(mkImbind(checkpat($1), checkexp($3))); }
	|
        error { skipping++; } SEMI		{ picmd(mkInull()); skipping = 0; }
        ;

modids:	modid					{ $$ = lsing($1); }
	|
	modids lmdelim modid			{ $$ = mklcons($3, $1); }
	;
lmdelim	: COMMA | /* empty */ ;

what:	varcon					{ $$ = $1; }
	|
	LPAR LBRA varcon RBRA RPAR		{ $$ = listid; }
	;

declcmd: rtopdecl				{ $$ = $1; }
	;

declcmds:	declcmds SEMI declcmd	{ $$ = mkabind($1, $3); }
		|
		declcmd			{ $$ = $1; }
		;

load:	OLOAD { ininterface++; } limport LEOF 	
		{ 
		    g3qual($3) = qualload; 
		    picmd(mkIcload(mklcons(mkppair($1, mklnil()), 
					   autoloads), 
				   $3, loadimport)); 
		    ininterface--; 
		    addinfixes($3);
		}
	|
	MLOAD module LEOF 		{ picmd(mkImload($1, root)); }
	|
    	ERREOF				{ picmd(mkInull()); }
	;
limport:	INTERFACE modid Where LCURL iimports o_fixes itopdecls rcurl { $$ = mkimport($2, nrev(&$5), nrev(&$6), nrev(&$7), mkispec(0, Lnil),  Lnil, 0, Lnil); }
		;

sinterface:	INTERFACE modid Where LCURL iimports o_fixes itopdecls rcurl	 { iroot = mkinterface($2, nrev(&$5), nrev(&$6), nrev(&$7)); }
		;

/*********/

module:		MODULE modid o_exportlist Where LCURL
			 o_imports o_fixes topdecls rcurl
			{ chkmodname($2); root = mkhmodule($2, $3, nrev(&$6), nrev(&$7), $8); }
		|
		MODULE modid o_exportlist Where LCURL
			 imports rcurl
			{ chkmodname($2); root = mkhmodule($2, $3, nrev(&$6), Lnil, mknbind()); }
		|
		LCURL o_imports o_fixes topdecls rcurl
			{ impstuff exps;
			  if (h1_3)
			      exps = mkisome(lsing(mkexpid("_main")));
			  else
			      exps = mkinone();
			  chkmodname("_Main"); 
			  root = mkhmodule("_Main", exps, nrev(&$2), nrev(&$3), $4); }
		;



modid:		conid			{ $$ = $1; }
		|
    		varid			{ $$ = $1; pedchk("lower case module name"); }
		|
		STRING			{ char *p = malloc(strlen($1)+2);
					  p[0] = '_';
					  strcpy(p+1, $1);
					  $$ = p;
					  pedchk("string module name"); }
		;

imports:	imports1		{ $$ = $1; }
		|
    		/* empty */		{ $$ = Lnil; }
		;
o_imports:	/* empty */		{ $$ = Lnil; }
		|
		imports1 SEMI		{ $$ = $1; }
		;

imports1:	imports1 SEMI import	{ $$ = mklcons($3, $1); }
		|
		import			{ $$ = lsing($1); }
		;
import:		IMPORT 
		o_qualified 
		modid 
		{ 
		  if (!switchtoid(!h1_3 && strcmp($3, "_Prelude") == 0 ? empty_prel : $3)) getout(); 
		  ininterface++; 
		}
		importr 
		{ ininterface--; 
		  addinfixes($5); 
		  g3qual($5) = $2; 
		  $$ = $5;
		  if (strcmp($3, "_Prelude") == 0 && g3as($5) != Lnil)
		      yyerror("Cannot rename Prelude on import");
		}
		;

importr:	INTERFACE modid Where LCURL iimports o_fixes itopdecls rcurl { allowas = allowasx; } LEOF o_as impspec o_renaming
	{ $$ = mkimport($2, nrev(&$5), nrev(&$6), nrev(&$7), $12, $13, 0, $11); }

o_qualified:	QUALIFIED { $$ = 1; allowasx = 1; } | { $$ = 0; allowasx = 0; };
o_as:		AS_KWD modid { $$ = lsing($2); allowas = 0; } | { $$ = Lnil; allowas = 0; };

impspec:	/* empty */		{ $$ = mkispec(0, Lnil); }
    		|
		importelist		{ $$ = mkispec(1, nrev(&$1)); }
		|
		LPAR RPAR		{ $$ = mkispec(1, Lnil); }
		|
		HIDING importelist	{ $$ = mkispec(0, nrev(&$2)); }
		;

iimports:	/* empty */		{ $$ = Lnil; }
		|
		iimports iimport	{ $$ = mklcons($2, $1); }
		;
iimport:	IMPORT modid exportlist o_renaming SEMI { $$ = mkimpimport($2, nrev(&$3), $4); }
		;

itopdecls:	itopdecl1s		{ $$ = $1; }
/*		|
				{ $$ = Lnil; }*/
		|
		SEMI			{ $$ = Lnil; }
		;
itopdecl1s:	itopdecl1s SEMI itopdecl { $$ = mklcons($3, $1); }
		|
		itopdecl		{ $$ = lsing($1); }
		;


itopdecl:	TYPE simple EQ type		{ $$ = mkimpsyn($2, $4); adderrinfo(typname($2)); }
		|
		DATA con_simple typinfo		{ $$ = mkimptype($2, mkinone(), $3); adderrinfo(typname($2)); }
		|
		DATA con_simple EQ { allowtypeexcl=1; } constrs o_deriving { $$ = mkimpeqtype($2, nrev(&$5), $6); adderrinfo(typname($2)); allowtypeexcl = 0;}
		|
		NEWTYPE con_simple EQ { allowtypeexcl=1; } constr1	{ $$ = mkimpisotype($2, lsing($5), mkinone()); adderrinfo(typname($2)); allowtypeexcl=0; } 
		|
                VIEW con_simple OF type EQ constrs	{ $$ = mkimpview($2, $4, $6); adderrinfo(typname($2)); }
                |
		CLASS con_class o_where_iclass	{ $$ = mkimpclass($2, $3, Lnil); adderrinfo(typname($2)); }
		|
		INSTANCE con_inst derinfo iinstmod istrinfos	{ $$ = mkimpinst($2, $3, $4, $5); }
		|
		names1 DCOLON type finfo		{ $$ = mkimpids($1, $3, $4); }
		|
		names1 DCOLON context DARROW type finfo	{ $$ = mkimpids($1, mktcontext($3, $5), $6); }
		;
iinstmod:	A_FROMMODULE modid ENDANNOT		{ $$ = $2; }
		|
		/* empty */				{ $$ = "_"; }
		;

typinfo:	/* empty */			{ $$ = mkinone(); }
                |
                A_FLAT int ENDANNOT	        { $$ = mkitypinfo(atoi($2), 1); }
	        ;

derinfo:	/* empty */			{ $$ = 0; }
                |
		A_DERIVED ENDANNOT	        { $$ = 1; }
                ;

istrinfos:	/* empty */			{ $$ = Lnil; }
		|
		istrinfos istrinfo		{ $$ = mklcons($2, $1); }
		;
istrinfo:	A_STRICTNESS aid EQ STRING conid ENDANNOT { $$ = mkppair($2, 
			mkhfinfo(0, $4, Lnil, -1, -1, 0, 0)); }
		;


o_deriving:	/* empty */		{ $$ = mkinone(); }
		|
		DERIVING CNAME		{ $$ = mkisome(lsing($2)); }
		|
		DERIVING LPAR names RPAR { $$ = mkisome($3); }
		;

finfo	: /* missing */			{ $$ = mknofinfo(); } |
          ipragmas			{ $$ = $1; }
	  ;

o_where_iclass:	/* empty */		{ $$ = mknbind(); }
		|
    		Where LCURL rcurl { $$ = mknbind(); }
		|
    		Where LCURL icdecls rcurl { $$ = $3; }
		;
icdecls:	icdecl			{ $$ = $1; }
		|
		icdecls SEMI icdecl	{ $$ = mkabind($1, $3); }
		;
icdecl:		names1 DCOLON type	{ $$ = mksbind($1, $3); }
		|
		names1 DCOLON context DARROW type	{ $$ = mksbind($1, mktcontext($3, $5)); }
		;

o_exportlist:	exportlist		{ $$ = mkisome(nrev(&$1)); }
		|
		/* empty */		{ $$ = mkinone(); }
		;
o_renaming:	RENAMING LPAR renamings RPAR	{ $$ = $3; }
		|
		/* empty */		{ $$ = Lnil; }
		;

exportlist:	LPAR exports RPAR	{ $$ = $2; }
                |
                LPAR RPAR               { $$ = Lnil; }
		;
exports:	export			{ $$ = lsing($1); }
		|
		exports COMMA export	{ $$ = mklcons($3, $1); }
		;

export:		ename			{ $$ = mkexpid($1); }
		|
		ename LPAR DOTDOT RPAR	{ $$ = mkexppdd($1); }
		|
		ename DOTDOT		{ check13("M..", 0); $$ = mkexpdd($1); }
		|
		MODULE ename		{ check13("module M", 1); $$ = mkexpdd($2); }
		|
		ename LPAR enames RPAR	{ $$ = mkexpl($1, $3); }
		;

importelist:	LPAR importes RPAR	{ $$ = $2; }
		;
importes:	importe			{ $$ = lsing($1); }
		|
		importes COMMA importe	{ $$ = mklcons($3, $1); }
		;

importe:	ename			{ $$ = mkexpid($1); }
		|
		ename LPAR DOTDOT RPAR	{ $$ = mkexppdd($1); }
		|
		ename LPAR enames RPAR	{ $$ = mkexpl($1, $3); }
		;

renamings:	renaming		{ $$ = lsing($1); }
		|
		renamings COMMA renaming	{ $$ = mklcons($3, $1); }
		;
renaming:	name TO name		{ $$ = mkirename($1, $3); }
		|
		name TO WILD		{ $$ = mkirename($1, "_"); }
		;

ename:		name			{ $$ = $1; }
		;
name:		varcon			{ $$ = $1; }
		;
names:		/* empty */	{ $$ = Lnil; }
		|
		names1		{ $$ = $1; }
		;
names1:		rnames1		{ $$ = nrev(&$1); }
		;
rnames1:	name		{ $$ = lsing($1); curid = $1; }
		|
		rnames1 COMMA name { $$ = mklcons($3, $1); }
		;
enames:		/* empty */	{ $$ = Lnil; }
		|
		enames1		{ $$ = nrev(&$1); }
		;
enames1:	ename		{ $$ = lsing($1); }
		|
		enames1 COMMA ename { $$ = mklcons($3, $1); }
		;

o_fixes:	o_fixes fix		{ $$ = mklcons($2, $1); }
		|
		/* empty */		{ $$ = Lnil; }
		;
fix:		INFIXL o_digit ops SEMI	{ if (!ininterface) makefixopl($3, INFIXL, $2); $$ = mkifix($3, 1, $2); }
		|
		INFIXR o_digit ops SEMI	{ if (!ininterface) makefixopl($3, INFIXR, $2); $$ = mkifix($3, 2, $2); }
		|
		INFIX o_digit ops SEMI	{ if (!ininterface) makefixopl($3, INFIX, $2); $$ = mkifix($3, 0, $2); }
		|
		MNONFIX ops SEMI	{ if (!ininterface) makefixopl($2, NONFIX, 9); $$ = mkifix($2, 5, 9); }
		;
o_digit:	DIGIT			{ $$ = atoi($1); }
		|
		/* empty */		{ $$ = 9; }
		;

ops:		ops COMMA iop		{ $$ = mklcons($3, $1); }
		|
		iop			{ $$ = lsing($1); }
		;
iop:		op			{ $$ = $1; }
		|
		STRING			{ $$ = $1; pedchk("arbitrary operator"); }
		;

topdecls:	topdecls SEMI topdecl	{ $$ = mkabind($1, $3); }
		|
		topdecl			{ $$ = $1; }
		;
rtopdecl:	TYPE simple EQ type		{ $$ = mkebind($2, $4); adderrinfo(typname($2)); }
		|
		DATA con_simple EQ { allowtypeexcl=1;} constrs o_deriving	{ $$ = mktbind($2, nrev(&$5), $6); adderrinfo(typname($2)); allowtypeexcl = 0; }
		|
		NEWTYPE con_simple EQ { allowtypeexcl=1; } constr1 o_deriving	{ $$ = mkxbind($2, lsing($5), $6); adderrinfo(typname($2)); allowtypeexcl=0; } 
		|
                VIEW con_simple OF type EQ constrs Where LCURL decls rcurl	{ $$ = mkvbind($2, $4, $6, $9); adderrinfo(typname($2)); }
                |
		CLASS con_class o_where_class	{ $$ = mkcbind($2, $3); adderrinfo(typname($2)); }
		|
		INSTANCE con_inst o_where_inst	{ $$ = mkibind($2, $3); }
		|
		DEFAULT LPAR RPAR		{ $$ = mkubind(Lnil); }
		|
		DEFAULT LPAR type RPAR		{ $$ = mkubind(lsing($3)); }
		|
/*		DEFAULT typeid			{ check13("default <type>", 0); $$ = mkubind(lsing($2)); }
    		|
*/
		DEFAULT LPAR types RPAR		{ $$ = mkubind(nrev(&$3)); }
		|
		A_SPECIALIZE varcon DCOLON iprtypevals ENDANNOT { $$ = mkspecbind($2, $4); }
		|
		A_SPECIALIZE INSTANCE con_inst ENDANNOT { $$ = mkspecinstbind($3); }
		;
topdecl:	rtopdecl			{ $$ = $1; }
		|
		decl				{ $$ = $1; }
		;
o_where_inst:	/* empty */			{ $$ = mknbind(); }
		|
		Where LCURL decls rcurl		{ $$ = $3; }
		;
o_where_class:	Where LCURL clsigns rcurl	{ $$ = $3; }
		|
		/* empty */			{ $$ = mknbind();}
		;
con_simple:	simple				{ $$ = $1; }
		|
		context DARROW simple		{ $$ = mktcontext($1, $3); }
		;
con_class:	class				{ $$ = $1; }
		|
		context DARROW class		{ $$ = mktcontext($1, $3); }
		;

con_inst:	tycon inst			{ $$ = mktname($1, lsing($2)); }
		|
		context DARROW tycon inst	{ $$ = mktcontext($1, mktname($3, lsing($4))); }
		;

inst:		type			{ $$ = checkinst($1); }
		;
decls:		decls SEMI decl		{ $$ = mkabind($1, $3); }
		|
		decl			{ $$ = $1; }
		;
decl:		names1 DCOLON type	{ $$ = mksbind($1, $3); }
		|
		names1 DCOLON context DARROW type	{ $$ = mksbind($1, mktcontext($3, $5)); }
		|
		valdef			{ $$ = $1; }
		;
		
valdef:		funbind			{ $$ = $1; }
		;

type:		atype1			{ $$ = $1; }
		|
		type ARROW type		{ $$ = mktname($2, ldub($1, $3)); }
                |
	        tycon			{ $$ = mktname($1, Lnil); }
		|
		class_or_type		{ $$ = $1; }
		|
		tycon atypes2		{ $$ = mktname($1, nrev(&$2)); }
		|
		tyvar atypes1		{ $$ = mktap($1, nrev(&$2)); check13("higher kinds", 1); }
		;

atypes2:	atypes2 atype		{ $$ = mklcons($2, $1); }
		|
		atype atype		{ $$ = ldub($2, $1); }
		;

atypes1:	atypes1 atype		{ $$ = mklcons($2, $1); }
		|
		atype			{ $$ = lsing($1); }
		;

class_or_type:	tycon atype		{ $$ = mktname($1, lsing($2)); }
		;

atype:		tycon			{ $$ = mktname($1, Lnil); }
		|
		atype1			{ $$ = $1; }
		;

atype1:		tyvar			{ $$ = $1; }
		|
		context_or_type		{ $$ = $1; }
		|
		LBRA type RBRA		{ $$ = mktname(listid, lsing($2)); }
		;
types:		types COMMA type	{ $$ = mklcons($3, $1); }
		|
		type COMMA type		{ $$ = ldub($3, $1); }
		;

context_or_type: LPAR types RPAR	{ $$ = mktname(tupstr($2, 0), nrev(&$2)); }
		|
		LPAR type RPAR		{ $$ = $2; }
		;

simple:		tycon tyvars		{ $$ = mktname($1, $2); }
		|
		LPAR tyconk DCOLON kind RPAR tyvars	{ $$ = mktnamek($2, $4, $6); allowstar = 0; }
		;
tyvars:		tyvar tyvars		{ $$ = mklcons($1, $2); }
		|
		/* empty */		{ $$ = Lnil; }
		;

kind:		kind ARROW kind		{ $$ = mkkarrow($1, $3); }
		|
		LPAR kind RPAR		{ $$ = $2; }
		|
		STAR			{ $$ = mkkground(); }
		;

constrs:	constrs BAR constr	{ $$ = mklcons($3, $1); }
		|
		constr			{ $$ = lsing($1); }
		;

constr1:	name atype		{ $$ = mkatc($1, Lnil, lsing($2)); }
		|
		context_or_type DARROW name atype	{ $$ = mkatc($3, checkcontext($1), lsing($4)); }
		;

constr:		context_or_type DARROW name satypes	{ $$ = mkatc($3, checkcontext($1),$4); }
                |
/*		class_or_type DARROW name satypes	{ $$ = mkatc($3, checkclass($1), $4); }
                |*/
                name satypes		{ $$ = mkatc($1, Lnil, $2); }
		|
		satype opx stype	{ $$ = mkatc($2, Lnil, ldub($1, $3)); }		/* satype should be stype, but this does not parse correctly */
		;

satypes:	satypesl		{ $$ = nrev(&$1); }
		;
sels1:		sel COMMA sels1		{ $$ = mklcons($1, $3); }
		|
		sel			{ $$ = lsing($1); }
		|
		sel COMMA		{ $$ = lsing($1); }
		;
sel:		varids1 DCOLON stype	{ $$ = mkppair($1, $3); }
		;
varids1:	varid COMMA varids1	{ $$ = mklcons($1, $3); }
		|
		varid			{ $$ = lsing($1); }
		;

satypesl:	satypesl xsatype	{ $$ = mklcons($2, $1); }
		|
		/* empty */		{ $$ = Lnil; }
		;

xsatype:	satype			{ $$ = $1; }
		|
		LCURL sels1 RCURL	{ check13("record", 1); $$ = mktsels($2); }
		;

satype:		atype			{ $$ = $1; }
		|
		atype aexcl		{ $$ = mktstrict($1); }
		|
		rexcl atype		{ $$ = mktstrict($2); }
		;

stype:		type			{ $$ = $1; }
		|
		type aexcl		{ $$ = mktstrict($1); }
		|
		rexcl type		{ $$ = mktstrict($2); }
		|
		LPAR type aexcl RPAR	{ $$ = mktstrict($2); }
		;

aexcl:		A_STRICT ENDANNOT
		;
rexcl:		TYPEEXCL;

/* This very yucky, but YACC isn't powerful enough to distinguish C => T and T due to tuple types
** and parenthesised contexts */
context:	context_or_type		{ $$ = checkcontext($1); }
		|
		class			{ $$ = lsing($1); }
		;

class:		tycon tyvar		{ $$ = mktname($1, lsing($2)); }
		|
		LPAR tyconk DCOLON kind RPAR tyvar	{ $$ = mktnamek($2, $4, lsing($6)); allowstar = 0; }
		;
/*
ctyvars:	tyvar ctyvars1		{ $$ = mklcons($1, $2); if ($2 != Lnil && !allowmulti) syntaxerror();}
ctyvars1:				{ $$ = Lnil; }
		tyvar ctyvars1		{ $$ = mklcons($1, $2); }
*/
tyconk:		tycon  { allowstar=1; $$ = $1; }
		;

clsigns:	clsigns SEMI clsign	{ $$ = mkabind($1, $3); }
		|
		clsign			{ $$ = $1; }
		;

clsign:		names DCOLON type	{ $$ = mksbind($1, $3); }
		|
		names DCOLON context DARROW type	{ $$ = mksbind($1, mktcontext($3, $5)); }
		|
		valdef			{ $$ = $1; }
		;

funbind:	pat EQ exp		{ $$ = mkpbind(lsing(mkppat($1, $3))); adderrinfo(leftmostid($1)); }
		|
		pat EQ exp WHERE LCURL decls rcurl { $$ = mkpbind(lsing(mkppat($1, mkletv(mkrbind($6), $3)))); adderrinfo(leftmostid($1)); }
		|
		pat fungdp1 awhere 	{ $$ = mkpbind(lsing(mkppat($1, mkwherev($2, $3)))); adderrinfo(leftmostid($1)); }
		;

fungdp1:	fgdp fungdp		{ $$ = mklcons($1, $2); }
		;
fungdp:		fgdp fungdp		{ $$ = mklcons($1, $2); }
		|
		/* empty */		{ $$ = Lnil; }
		;
fgdp:		gd EQ exp		{ $$ = mklcons($1, $3); /* misuse! */ }
		;

o_gd:		gd			{ $$ = $1; }
		|
		/* empty */		{ $$ = 0; }
		;

apats:		apats apat		{ $$ = mklcons($2, $1); }
		|
		apat			{ $$ = lsing($1); }
		;

exp:		xexp			{ $$ = checkexp($1); }
		;

/* xexps is either an expression or a pattern, use exp or epat to get the check */
xexp:		aexp			{ $$ = $1; }
		|
		xexp aexp	%prec PREC_AP	{ $$ = mkap($1, $2); }
		|
		xexp OPN0 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPN1 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPN2 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPN3 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPN4 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPN5 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPN6 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPN7 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPN8 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPN9 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPL0 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPL1 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPL2 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPL3 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPL4 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPL5 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp MINUS xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPL6 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPL7 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPL8 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPL9 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPR0 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPR1 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPR2 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPR3 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPR4 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPR5 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPR6 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPR7 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPR8 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		xexp OPR9 xexp		{ $$ = mkbinop($2, $1, $3); }
		|
		MINUS xexp %prec UMINUS	{ $$ = mkunop(h1_3 && 0 ? "_Prelude.negate" : "_negate", $2); }
		|
		BACKSLASH apats o_gd ARROW xexp %prec PREC_LAM	{ $$ = mklams(nrev(&$2), $3, $5); }
		|
		IF xexp THEN xexp ELSE xexp %prec IF	{ $$ = mkterop("Pif", $2, $4, $6); }
                |
                let_left IN xexp %prec LET	{ $$ = mkletv($1, $3); }
		|
		xexp OLD_WHERE LCURL decls rcurl	{ $$ = mkletv(mkrbind($4), $1); }
		|
		xexp DCOLON rtype		{ $$ = mkrestr($1, $3); }
		|
		xexp DCOLON context_or_type DARROW rtype %prec DCOLON { $$ = mkrestr($1, mktcontext(checkcontext($3), $5)); }
		|
		xexp DCOLON class_or_type DARROW rtype	%prec DCOLON { $$ = mkrestr($1, mktcontext(checkclass($3), $5)); }
		|
	        aexp epragma %prec P_ANNOT	{ $$ = mkeannot($1, $2); }
		|
		DO LCURL stmt rcurl		{ $$ = mkdoexp($3); }
		;

stmt:		xexp				{ $$ = mkstmtexp(checkexp($1)); }
		|
		xexp SEMI stmt			{ $$ = mkstmtexpstmt(checkexp($1), $3); }
		|
		let_left SEMI stmt		{ $$ = mkstmtlet($1, $3); }
		|
		xexp LARROW xexp SEMI stmt	{ $$ = mkstmtbind(checkpat($1), checkexp($3), $5); }
		;

let_left:	LET LCURL decls rcurl		{ $$ = mkrbind($3); }
		;

rtype:		atype				{ $$ = $1; }
		;

aexp:		raexp		{ $$ = $1; }
		|
		TILDE rapat	{ $$ = mklazyp($2); }
		|
		raexp LCURL efields RCURL %prec PREC10 { check13("record", 1); $$ = mkrecord($1, $3); }
		;

raexp:		name		{ $$ = mkidentu($1); }
		|
		name AS apat	{ $$ = mkas($1, $3); }		/* pattern */
		|
		int		{ $$ = mkbignum($1); }
		|
		CHAR		{ $$ = mkcharr($1); }
		|
		STRING		{ $$ = mkstring($1); }
		|
		INTCONST	{ $$ = mkinteger($1); }
		|
		RATNUM		{ $$ = mkratnum($1); }
		|
		LPAR xexp RPAR	{ $$ = $2; }
		|
		LPAR texps RPAR	{ $$ = mktuple(nrev(&$2)); }
		|
		LBRA xexp RBRA	{ $$ = mkcons($2, niltree); }
		|
		LBRA texps RBRA	{ $$ = mlist($2); }
		|
		LBRA xexp DOTDOT RBRA	{ $$ = mklistf(L_FROM, lsing($2)); }
		|
		LBRA xexp COMMA xexp DOTDOT RBRA	{ $$ = mklistf(L_FROM_BY, ldub($2, $4)); }
		|
		LBRA xexp DOTDOT xexp RBRA	{ $$ = mklistf(L_FROM_TO, ldub($2, $4)); }
		|
		LBRA xexp COMMA xexp DOTDOT xexp RBRA	{ $$ = mklistf(L_FROM_BY_TO, mklcons($2, ldub($4, $6))); }
		|
		LBRA xexp BAR qual RBRA	{ $$ = mklistg($2, $4); }
                |
		CASE xexp OF LCURL alts rcurl %prec CASE	{ $$ = mkcasee($2, nrev(&$5)); }
		|
    		WILD		{ $$ = mkident("_"); }			/* pattern! */
		|
		LPAR xexp varop RPAR	{ $$ = mkap(mkident($3), $2); }
		|
		LPAR binop1 xexp RPAR	{ $$ = mksection($2, $3); }
		;

efields:	/* empty */ 		{ $$ = Lnil; }
		|
		efields1
		;
efields1:	efield COMMA efields	{ $$ = mklcons($1, $3); }
		|
		efield			{ $$ = lsing($1); }
		;
efield:		name EQ xexp		{ $$ = mkppair($1, $3); }
		|
		name 			{ $$ = mkppair($1, mkidentu($1)); }
		;

texps:		texps COMMA xexp	{ $$ = mklcons($3, $1); }
		|
		xexp COMMA xexp		{ $$ = ldub($3, $1); }
		;

gd:		BAR xexp		{ $$ = $2; }
		;

qual:		qualn			{ $$ = nrev(&$1); }

qualn:		qualn COMMA qual1	{ $$ = mklcons($3, $1); }
		|
		qual1			{ $$ = lsing($1); }
		;
qual1:		epat LARROW xexp	{ $$ = mkqgen($1, $3); }
		|
		xexp			{ $$ = mkqfilter($1); }
		|
		let_left		{ check13("let in list comprehension", 1); $$ = mkqlet($1); }
		;
epat:		xexp			{ $$ = checkpat($1); }
		;

alts:		alts SEMI alt		{ $$ = lconc($3, $1); }
		|
		alt			{ $$ = $1; }
		;

alt:		pat ARROW exp		{ $$ = lsing(mkppat($1, $3)); }
		|
		pat ARROW exp WHERE LCURL decls rcurl { $$ = lsing(mkppat($1, mkletv(mkrbind($6), $3))); }
		|
		pat altgdp1 awhere	{ 
/*
		                             list l;
					     for(l = $2; tlist(l) == lcons; l = ltl(l)) {
						 list pe = (list)lhd(l);
						 lhd(l) = (int *)mkppat(mkcondp($1, lhd(pe)), ltl(pe));
					     }
					     $$ = $2;
*/
		                             $$ = lsing(mkppat($1, mkwherev($2, $3)));
		                        }
		;
altgdp1:	gdp altgdp		{ $$ = mklcons($1, $2); }
		;
altgdp:		gdp altgdp		{ $$ = mklcons($1, $2); }
		|
		/* empty */		{ $$ = Lnil; }
		;
gdp:		gd ARROW exp		{ $$ = mklcons($1, $3); /* misuse! */ }
		;
awhere:		WHERE LCURL decls rcurl { $$ = $3; }
		|
		/* empty */		{ $$ = mknbind(); }
		;

pat:		apat			{ $$ = $1; }
		|
    		apat apats		{ $$ = mkapchain($1, $2); }
		|
		pat OPN0 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPN1 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPN2 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPN3 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPN4 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPN5 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPN6 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPN7 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPN8 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPN9 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPL0 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPL1 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPL2 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPL3 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPL4 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPL5 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat MINUS pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPL6 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPL7 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPL8 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPL9 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPR0 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPR1 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPR2 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPR3 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPR4 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPR5 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPR6 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPR7 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPR8 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		pat OPR9 pat		{ $$ = mkbinop($2, $1, $3); }
		|
		MINUS int %prec UMINUS	{ $$ = mkbignum(negbig($2)); }
		|
		MINUS RATNUM %prec UMINUS	{ $$ = mkratnum(negbig($2)); }
		;

apat:		TILDE rapat		{ $$ = mklazyp($2); }
		|
                apat1			{ $$ = $1; }
		;

apat1:		rapat			{ $$ = $1; }
		|
		conid LCURL pfields RCURL %prec PREC10 { check13("record", 1); $$ = mkrecord(mkident($1), $3); }
		;

rapat:		name			{ $$ = mkident($1); }
		|
		name AS apat1		{ $$ = mkas($1, $3); }
		|
		int			{ $$ = mkbignum($1); }
		|
		RATNUM			{ $$ = mkratnum($1); }
		|
		CHAR			{ $$ = mkcharr($1); }
		|
		STRING			{ $$ = mkstring($1); }
		|
		WILD			{ $$ = mkident("_"); }
		|
		LPAR pat RPAR		{ $$ = $2; }
		|
		LPAR tpats RPAR		{ $$ = mktuple(nrev(&$2)); }
		|
		LBRA lpats RBRA		{ $$ = mlist($2); }
		;
tpats:		tpats COMMA pat		{ $$ = mklcons($3, $1); }
		|
		pat COMMA pat		{ $$ = ldub($3, $1); }
		;
lpats:		lpats COMMA pat		{ $$ = mklcons($3, $1); }
		|
		pat			{ $$ = lsing($1); }
		;

pfields:	/* empty */		{ $$ = Lnil; }
		|
		pfields1
		;
pfields1:	pfield COMMA pfields	{ $$ = mklcons($1, $3); }
		|
		pfield			{ $$ = lsing($1); }
		;
pfield:		name EQ pat		{ $$ = mkppair($1, $3); }
		|
		name			{ $$ = mkppair($1, mkident($1)); }
		;

varid:		NAME		{ $$ = $1; }
		|
		LPAR varop RPAR	{ $$ = $2; }
		|
		LPAR ARROW RPAR { $$ = installid("P->"); }
		;
varop:		binop1 { $$ = $1; } | MINUS{ $$ = $1;} /* | ARROW { $$ = installid("P->"); }*/ ;
binop1:		opx { $$ = $1; } /* | COMMA{$$="_,"; pedchk("(,)"); } */;
opx:		OPN0{$$=$1;}|OPN1{$$=$1;}|OPN2{$$=$1;}|OPN3{$$=$1;}|OPN4{$$=$1;}|
		OPN5{$$=$1;}|OPN6{$$=$1;}|OPN7{$$=$1;}|OPN8{$$=$1;}|OPN9{$$=$1;}|
		OPL0{$$=$1;}|OPL1{$$=$1;}|OPL2{$$=$1;}|OPL3{$$=$1;}|OPL4{$$=$1;}|
		OPL5{$$=$1;}|OPL6{$$=$1;}|OPL7{$$=$1;}|OPL8{$$=$1;}|OPL9{$$=$1;}|
		OPR0{$$=$1;}|OPR1{$$=$1;}|OPR2{$$=$1;}|OPR3{$$=$1;}|OPR4{$$=$1;}|
		OPR5{$$=$1;}|OPR6{$$=$1;}|OPR7{$$=$1;}|OPR8{$$=$1;}|OPR9{$$=$1;};
conid:		CNAME		{ $$ = $1; } | speccon;
op:		varop		{ $$ = $1; };
varcon:		varid		{ $$ = $1; } | conid { $$ = $1; };
int:		DIGIT { $$=$1; } | INT { $$=$1; };
tycon:		conid		{ $$ = $1; } | LPAR ARROW RPAR { $$ = installid("P->"); } ;
tyvar:		NAME		{ $$ = mktvar(typeno($1)); }

speccon:	LPAR RPAR	{ $$ = installid("_()"); }
		|
		LBRA RBRA	{ $$ = installid("_[]"); }
		|
		LPAR commas1 RPAR { char b[10]; sprintf(b, "P#%d", $2+1); $$ = installid(b); }
		;

commas1:	COMMA		{ $$ = 1; }
		|
		COMMA commas1	{ $$ = $2+1; }
		;

rcurl:		SEMI rcurl0	/* allow extra ; at the end */
		|
    		rcurl0
    		;

/*
rcurl0:		{ wantrcurl = 1; } rcurl1 { wantrcurl = 0; }
		;
*/
rcurl0:		RCURL
                |
/* These tokens would cause an error, try to insert a soft } */
		Badtokens	{ yyclearin; popsoft(); pushlast(); }
/*
    		|
		error		{ yyerrok; popsoft(); }
*/
    		;

/* BAR cause many s/r conflicts */
Badtokens:	IN | RPAR | RBRA | COMMA | DOTDOT | THEN | ELSE | /*BAR |*/ OF | SEMI;

/*o_int:		int | *** empty */

varids:		/* empty */ { $$ = Lnil; } | varids varid { $$ = mklcons($2,$1); };

strings:	/* empty */ { $$ = Lnil; } | strings STRING { $$ = mklcons($2,$1); };

/*
aline:		A_LINE int o_int STRING ENDANNOT
                ;
*/

ipragmas:	ipragma {$$=$1;} | ipragmas ipragma {$$=joinpragma($1,$2);} ;

ipragma:	A_INLINE varcon varids EQ exp ENDANNOT {
				tree et = $5;
				list vl = $3;
				chkcurid($2);
				while(tlist(vl) == lcons) {
				    et = mklam(mkident(lhd(vl)), et);
				    vl = ltl(vl);
				}
				$$ = mkhfinfo(et, "", Lnil, -1, -1, 0, 0);
			}
		|
		A_STRICTNESS aid EQ STRING conid ENDANNOT {
				char *con = $5;
				char *curs = "";
				if (!zyzflag)
				    con++;
				chkcurid($2);
				if (strcmp(con, "ST") == 0)
				    curs = $4;
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
				    curs = installid(q);
				} else {
				    curs = "";
				    /* unknown strictness info */
				}
				$$ = mkhfinfo(0, curs, Lnil, -1, -1, 0, 0);
			}
		|
		A_ENTRY aid EQ strings ENDANNOT {
				chkcurid($2);
				$$ = mkhfinfo(0, "", $4, -1, -1, 0, 0);
			}
		|
		A_ARITY aid EQ int ENDANNOT {
				chkcurid($2);
				$$ = mkhfinfo(0, "", Lnil, atoi($4), -1, 0, 0);
			}
		|
		A_FRAMESIZE aid EQ int ENDANNOT {
				chkcurid($2);
				$$ = mkhfinfo(0, "", Lnil, -1, atoi($4), 0, 0);
			}
                |
		A_SPECIALIZE aid DCOLON iprtypes ENDANNOT { 
		    		chkcurid($2);
				$$ = mkhfinfo(0, "", Lnil, -1, -1, $4, 0);
			}
		|
		A_EVALED aid ENDANNOT {
				chkcurid($2);
				$$ = mkhfinfo(0, "", Lnil, -1, -1, 0, 1);
			}
		;
aid:		varcon { $$ = $1; } | WILD { $$ = "_"; }
                ;

iprtypes:	iprtype		{ $$ = lsing($1); }
		|
		iprtypes COMMA iprtype { $$ = mklcons($3, $1); }
		;
iprtype:	type finfo		{ $$ = mklcons($1, $2); }
		;

iprtypevals:	iprtypeval		{ $$ = lsing($1); }
		|
		iprtypevals COMMA iprtypeval { $$ = mklcons($3, $1); }
		;
iprtypeval:	type oval		{ $$ = mklcons($1, $2); }
		;
oval:		/* empty */ 		{ $$ = 0; }
		|
		EQ varid		{ $$ = $2; }
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
		A_OVERLOAD ENDANNOT { $$ = "OVERLOAD"; }
		|
		A_NOTCHK ENDANNOT { $$ = "NOTCHK"; }
		;


Where:		WHERE|OLD_WHERE;

%%

binding
andthem(l)
list l;
{
	if (tlist(ltl(l)) == lnil)
		return (binding)lhd(l);
	else
		return mkabind(lhd(l), andthem(ltl(l)));
}

tree
mkapchain(f, args)
tree f;
list args;
{
	if (tlist(args) == lnil)
		return f;
	else
		return mkap(mkapchain(f, ltl(args)), lhd(args));
}

tree
mlist(l)
list l;
{
	tree r;

	for(r = niltree; tlist(l) != lnil; l = ltl(l))
		r = mkcons(lhd(l), r);
	return r;
}

#if 0
binding
fbind(p, gd, exp, gdl, wh)
tree p;
tree gd, exp;
list gdl;
binding wh;
{
    tree t;
    pbinding pr;
    binding bl;

    if (!wh) {
	t = mkcondp(p, gd);
	nrev(&gdl);
	bl = mkpbind(lsing(mkppat(t, exp)));
	for(; tlist(gdl) != lnil; gdl = ltl(gdl)) {
	    pr = mkppat(mkcondp(p, lhd(lhd(gdl))), ltl(lhd(gdl)));
	    bl = mkabind(bl, mkpbind(lsing(pr)));
	}
	return bl;
    } else {
	fprintf(stderr, "WHERE\n");
	exit(1);
    }
}
#endif

#if 0
id
funname(p)
pbinding p;
{
    tree t = gppat(p);
    while(ttree(t) == ap)
	t = gfun(t);
    return gident(t);
}

binding
bindl(l)
list l;
{
    /* take a list of function bindings and split it into the various functions */
    binding b, p;
    id fname;
    list fl;

	b = 0;
    while(tlist(l) != lnil) {
	fl = Lnil;
	for(fname = funname(lhd(l)); tlist(l) != lnil && strcmp(fname, funname(lhd(l))) == 0; l = ltl(l)) {
	    fl = mklcons(lhd(l), fl);
	}
	p = mkpbind(fl);
	b = b ? mkabind(p, b) : p;
    }
    return b;
}
#endif

int
allvars(l)
list l;
{
    for(; tlist(l) != lnil; l = ltl(l))
	if (tttype(lhd(l)) != tvar)
	    return 0;
    return 1;
}

ttype
checksimple(t)
ttype t;
{
    /* check that we have a type name with variables only */
    if (tttype(t) != tname || !allvars(gtypel(t)))
	syntaxerror();
    return t;
}

ttype
checkclass(t)
ttype t;
{
    list l;

    /* Check that we have a type constructor with a single type variable. */
    if (tttype(t) != tname || tlist(l = gtypel(t)) != lcons ||
	tlist(ltl(l)) != lnil || tttype(lhd(l)) != tvar)
	syntaxerror();
    return t;
}

void
unimpl()
{
    fprintf(stderr, "unimpl\n");
    exit(1);
}

int
switchtoid(i)
id i;
{
    char name[1024];
    extern char *incpath;
    static char *suffixes[] = { ".hi", ".hy", 0 };
    char *p, **s;
    FILE *f;

    strcpy(name, i+1);
    p = name + strlen(name);
    for(s = suffixes; *s; s++) {
	strcpy(p, *s);
	if ((f = pathopen(incpath, name, "r")) != NULL) {
	    pushfile(f, name);
	    return 1;
	}
    }
    errmsg("Cannot open interface file %s\n", i+1);
    return 0;
}

tree
mklams(l, g, e)
list l;
tree g, e;
{
    if (tlist(ltl(l)) == lnil)
	return mklam(cndp(lhd(l), g), e);
    else
	return mklam(lhd(l), mklams(ltl(l), g, e));
}

tree
cndp(p, c)
tree p, c;
{
    return c ? mkcondp(p, c) : p;
}

list
checkcontext(t)
ttype t;
{
    /* if t is a tuple type, extract list and check parts for class satisfaction */
    /* else it must be a single class */

    if (tttype(t) == tname && gtypeid(t)[1] == '#') {
	list p;
	
	for(p = gtypel(t); tlist(p) == lcons; p = ltl(p))
	    checkclass(lhd(p));
	return gtypel(t);
    } else {
	checkclass(t);
	return lsing(t);
    }
}

finfot
finfofromstring(s)
char *s;
{
    char args[100], res[10];
    int frs;

    args[0] = res[0] = '_';
    if (sscanf(s, ",%[TF],%d", res+1, &frs) == 2) {
	return mkfinfo("_", installid(res), frs);
    } else if (sscanf(s, "%[TF],%[TF],%d", args+1, res+1, &frs) == 3) {
	return mkfinfo(installid(args), installid(res), frs);
    } else if (sscanf(s, ",%[TF]", res+1) == 1) {
	return mkfinfo("_", installid(res), UFRS);
    } else if (sscanf(s, "%[TF],%[TF]", args+1, res+1) == 2) {
	return mkfinfo(installid(args), installid(res), UFRS);
    } else {
	return mknofinfo();
    }
}

id
negbig(s)
char *s;
{
    char buf[10000];

    buf[0] = '-';
    strcpy(&buf[1], s);
    return installid(buf);
}

static tree mkcons(h, t)
tree h, t;
{
	return mkap(mkap(mkident("_:"), h), t);
}

static ttype
checkinst(t)
ttype t;
{
    list p;

    if (tttype(t) != tname) {
	syntaxerror();
	return 0;
    }
    if (!pedantic)
	return t;
    for(p = gtypel(t); tlist(p) == lcons; p = ltl(p))
	if (tttype(lhd(p)) != tvar) {
	    syntaxerror();
	    return 0;
	}
    return t;
}

static char *
typname(t)
ttype t;
{
    switch(tttype(t)) {
    case tname:
	return gtypeid(t);
    case tcontext:
	return typname(gctype(t));
    default:
	return 0;
    }
}

static void
chkcurid(s)
char *s;
{
    if (strcmp(s, curid) != 0 && strcmp(s, "_") != 0) {
	yyerror("Pragma not for current id");
    }
}

void
chkmodname(name)
char *name;
{
    char *p, *q;

    if (!pedantic)
	return;
    if ((q = strrchr(filename, '/')))
	q++;
    else
	q = filename;
    p = strrchr(q, '.');
    if (p)
	*p = 0;
    if (strcmp(q, name+1) != 0 && strcmp(name+1, "Main") != 0)
	fprintf(stderr, "Warning, file name and module name do not agree \"%s\" - \"%s\"\n", q, name+1);
    if (p)
	*p = '.';
}

/*mkhfinfo(curinline, curstrict, curentries, curarity, curfrsize, curinsts, curevaled); }
static
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
}*/

static finfot
joinpragma(f1, f2)
finfot f1, f2;
{
    return mkhfinfo(pfinline(f1) ? pfinline(f1) : pfinline(f2),
		    *pfstrict(f1) ? pfstrict(f1) : pfstrict(f2),
		    tlist(pfentry(f1)) != lnil ? pfentry(f1) : pfentry(f2),
		    pfarity(f1) >= 0 ? pfarity(f1) : pfarity(f2),
		    pffrsize(f1) >= 0 ? pffrsize(f1) : pffrsize(f2),
		    pfinsts(f1) ? (pfinsts(f2) ? lconc(pfinsts(f1),pfinsts(f2)) : pfinsts(f1)) : pfinsts(f2),
		    pfevaled(f1) | pfevaled(f2));
}

tree
mkidentu(i)
id i;
{
    if (h1_3 && strcmp(i, "_undefined") == 0) {
        char b[2000];
	sprintf(b, "POSITION %d %s", undefline, filename);
	return mkeannot(mkident(i), installid(b));
    } else {
        return mkident(i);
    }
}

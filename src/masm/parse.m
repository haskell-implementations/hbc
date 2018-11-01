module -- parse
-- parse and translate one line
#include "../mcode/mcodedef_t.t"
export parse;
rec
    addr ["Vp"]			= Vp
||  addr ["$";n;"(";"Vp";")"]	= Vrel (cstoi n)
||  addr ['$'.n;"(";"Vp";")"]	= Vrel (cstoi n)
||  addr [n;"(";"Vp";")"]	= Vind (cstoi n)
||  addr ["Vpush"]		= pushV
||  addr ["Vpop"]		= popV
||  addr ["Sp"]			= Sp
||  addr ["$";n;"(";"Sp";")"]	= Srel (cstoi n)
||  addr ['$'.n;"(";"Sp";")"]	= Srel (cstoi n)
||  addr [n;"(";"Sp";")"]	= Sind (cstoi n)
||  addr ["Spush"]		= pushS
||  addr ["Spop"]		= popS
||  addr ["Hp"]			= hp
||  addr ["$";n;"(";"Hp";")"]	= hprel (cstoi n)
||  addr ['$'.n;"(";"Hp";")"]	= hprel (cstoi n)
||  addr [n;"(";"Hp";")"]	= hpind (cstoi n)
||  addr ["toH"]		= tohp
||  addr ['r'.r as (c._)] & (isdigit c)= reg (cstoi r)
||  addr ["$";n;"(";'r'.r;")"]	= regrel (cstoi r)(cstoi n)
||  addr ['$'.n;"(";'r'.r;")"]	= regrel (cstoi r)(cstoi n)
||  addr [n;"(";'r'.r;")"]	= regind (cstoi r) (cstoi n)
||  addr ["$";s as (c._)] = if isdigit c | c='-' then mkconst s else idlit s
||  addr ['$'.s as (c._)] = if isdigit c | c='-' then mkconst s else idlit s
||  addr ['#'.s]		= retaddr s
||  addr [s as c._] & (isdigit c | c='-') = const (cstoi s)
||  addr ['@'.s]		= glob s
||  addr [s]			= glob s
||  addr s			= fail ("addr: "@mix s " ")
and mkconst s = if mem '.' s then fconst (stof s) else const (cstoi s)
and tag "oeval"		= oeval
||  tag "ounwind"	= ounwind
||  tag "ojfun"		= ojfun
||  tag "ogettag"	= ogettag
||  tag s & (all isdigit s) = onumtag (cstoi s)
and cstoi ('-'.s) = - cstoi s
||  cstoi s = if ~ all isdigit s then fail ("non-numeric argument: '"@s@"'") else stoi s
and cc "eq"		= eq
||  cc "ne"		= ne
||  cc "lt"		= lt
||  cc "gt"		= gt
||  cc "le"		= le
||  cc "ge"		= ge
||  cc "lts"		= ltstack
||  cc "lth"		= ltheap
||  cc "gts"		= gtstack
||  cc "geh"		= geheap
||  cc "feq"		= dfeq
||  cc "fne"		= dfne
||  cc "flt"		= dflt
||  cc "fgt"		= dfgt
||  cc "fle"		= dfle
||  cc "fge"		= dfge
||  cc "sfeq"		= sfeq
||  cc "sfne"		= sfne
||  cc "sflt"		= sflt
||  cc "sfgt"		= sfgt
||  cc "sfle"		= sfle
||  cc "sfge"		= sfge
and ops = map addr o choplist (splitat ",")
and mop opr r = let [a;b;c] = ops r in [Mop3 opr a b c]
and mop2 opr r = let [a;b] = ops r in [Mop2 opr a b]
and stripq (_.s) = (f s
	where rec f ['"']        = []
	       || f ('\\'.'n'.s) = '\n'.f s
	       || f (c       .s) = c   .f s)

and parse (l.":".r) = Mlabel l.parse r
||  parse [".pragma"; s] = [Mpragma s]
||  parse [".data"] = [Mdata]
||  parse [".text"] = [Mtext]
||  parse [".export";s] = [Mexport s]
||  parse [".string";s] = [Mstring (stripq s)]
||  parse (".word".ws) = map Mword (ops ws)
||  parse [".float";s] = [Mdfloat s]
||  parse [".dfloat";s] = [Mdfloat s]
||  parse [".sfloat";s] = [Msfloat s]
||  parse [".malign"] = [Malign]
||  parse [".funbegin";s;n] = [Mfunbegin s (stoi n)]
||  parse [".funbegin";s] = [Mfunbegin s 1000]
||  parse [".funend"] = [Mfunend]
||  parse [".asm";a] = [Masm (stripq a) []]
||  parse (".asm".a.",".args) = [Masm (stripq a) (ops args)]
||  parse ["noop"] = [Mnoop]
||  parse ["call"; s] = [Mcall s]
||  parse ["jump"; s] = [Mjump s]
||  parse ["jumpf"; s] = [Mjumpf s]
||  parse ["return"] = [Mreturn]
||  parse ("adda".r) = let [s;d] = ops r in [Madda s d]
||  parse ("move".r) = let [s;d] = ops r in [Mmove s d]
||  parse ("movef".r) = let [s;d] = ops r in [Mmovedf s d]
||  parse ("movedf".r) = let [s;d] = ops r in [Mmovedf s d]
||  parse ("movesf".r) = let [s;d] = ops r in [Mmovesf s d]
||  parse ["call";t;"(";'r'.r;")"] = [Mcalltag (tag t) (cstoi r)]
||  parse ["jump";t;"(";'r'.r;")"] = [Mjumptag (tag t) (cstoi r)]
||  parse ("comp".r) = let [s;d] = ops r in [Mcompare s d]
||  parse ("compf".r) = let [s;d] = ops r in [Mcomparedf s d]
||  parse ("compdf".r) = let [s;d] = ops r in [Mcomparedf s d]
||  parse ("compsf".r) = let [s;d] = ops r in [Mcomparesf s d]
||  parse ("add".r) = mop add r
||  parse ("sub".r) = mop sub r
||  parse ("mul".r) = mop mul r
||  parse ("div".r) = mop div r
||  parse ("mod".r) = mop mod r
||  parse ("lsh".r) = mop btlsh r
||  parse ("rsh".r) = mop btrsh r
||  parse ("or".r)  = mop btor r
||  parse ("and".r) = mop btand r
||  parse ("lsh2".r) = mop2 btlsh r
||  parse ("rsh2".r) = mop2 btrsh r
||  parse ("or2".r)  = mop2 btor r
||  parse ("and2".r) = mop2 btand r
||  parse ("add2".r) = mop2 add r
||  parse ("sub2".r) = mop2 sub r
||  parse ("neg2".r) = mop2 neg r
||  parse ("mul2".r) = mop2 mul r
||  parse ("div2".r) = mop2 div r
||  parse ("mod2".r) = mop2 mod r
||  parse ("dadd".r) = mop dfadd r
||  parse ("dsub".r) = mop dfsub r
||  parse ("dmul".r) = mop dfmul r
||  parse ("ddiv".r) = mop dfdiv r
||  parse ("dadd2".r) = mop2 dfadd r
||  parse ("dsub2".r) = mop2 dfsub r
||  parse ("dneg2".r) = mop2 dfneg r
||  parse ("dmul2".r) = mop2 dfmul r
||  parse ("ddiv2".r) = mop2 dfdiv r
||  parse ("dneg".r) = mop2 dfneg r
||  parse ("itod".r) = mop2 itodf r
||  parse ("dtoi".r) = mop2 dftoi r
||  parse ("case".x) =
	let (a.y) = choplist (splitat ",") x in
	let (l.h.m.r) = map hd y in
	[Mcase (addr a) (cstoi l) (cstoi h) (cstoi m) r 0] -- bad
||  parse ("boolcc".c.",".r) = let [d] = ops r in [Mboolcc (cc c) d]
||  parse ['j'.c; s] = [Mjcond (cc c) s]
||  parse [""] = []
||  parse [] = []
||  parse r = fail ("masm: syntax error: "@mix r " ") --[Mcom ("Strange:"@(mix r " "))]
end

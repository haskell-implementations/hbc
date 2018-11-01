module -- id
--
-- handles the identifier type
--
#include "../misc/triple.t"
#include "../misc/flags.t"
#include "einfo_t.t"
#include "einfo.t"
#include "id_t.t"
#include "ttype.t"
#include "ttype_t.t"
#include "../funnos.h"

#define PrMore false

export pprid, prid, oprid, eqid, ltid, idtostr, dummyid, asmid, idi_varu, arity_of_id, framesize_of_id, mkpids, nprid,
       id_is_global, noorigname, id_is_visible, type_of_id, id_is_predef, id_no, isdummy, id_orignames, 
       id_fixity, id_isvar, id_ismethod, mknewid, mknewids, id_isclass, id_isinst, id_issyn, id_isconstr, preludename,
       id_metarity, id_metsel, updvis, id_visibility, updidname, inprelude, entries_of_id, dprid, getminame,
       tupstr, optparen, idtopstr, specialpre, getspec, preludeBuiltin, get_id_kind, dropqual,isiso;
rec
    idi_varu = idi_var var_unknown Onotype None
and
    noorigname = Noorigname
and
    dummyid = mkid 0 "_" idi_varu noorigname
and
    isdummy (mkids "_") = true
||  isdummy (mkid 0 _ _ _) = true
||  isdummy _ = false
and
    idtostr (mkids s) = s
||  idtostr (mkidi s _ _ _) = s
||  idtostr (mkid n s i _) = s
and idtopstr i = 			-- profiling name
    let s = unmangle (idtostr i)
    in  if s="" then trace ("Bad unmangle "@idtostr i) "?!" else s
and unmangle ('V'.'V'.'.'.cs) = cs
||  unmangle name = (unm name
	where rec unm ('M'.'M'.'`'.cs) = cs		-- Methods
              ||  unm ('D'.'D'.'`'.cs) = cs		-- Default methods
              ||  unm ('V'.'V'.'`'.cs) = cs		-- Default methods
              ||  unm ('_'.cs)         = cs	-- ordinary ids
              ||  unm (cs as ('P'._)) = cs	-- Predefined ids
              ||  unm (cs as ('D'._)) = cs	-- Predefined ids for ops on Double
              ||  unm (c.cs) = unm cs
              ||  unm _ = "XXXX") --trace ("unmangle failed for "@name) "?!")
and
    transid "$" = "$"		-- why??
||  transid l = (f1 l
where rec
    f1 "->" = "->"
||  f1 ('#'.l) = '#'.f l
||  f1 l = f l
and f "" = ""
||  f (c.l) & (isalnum c | c = '_') = c.f l
||  f (c.l) = 
    if ord c >= 256 then
        let c' = ord c in
        let c4 = c' % 16
        and c3 = (c' / 16) % 16
        and c2 = (c' / 256) % 16
        and c1 = (c' / 4096) % 16 in
        ['$'; 'u'; hex c1; hex c2; hex c3; hex c4] @ f l
    else
        ['$'; hex (ord c/16); hex (ord c%16)] @ f l
and hex n = select (n+1) "0123456789ABCDEF")
and preludename = "_Prelude"
and preludeBuiltin = if H1_3 then "_Prelude" else "_PreludeBuiltin"
and inprelude (MI ('_'.'P'.'r'.'e'.'l'.'u'.'d'.'e'._), _) = true
||  inprelude _ = false

and specialname ('P'._) = true
||  specialname ('D'._) = true
||  specialname _ = false
and islml ('_'.'L'.'M'.'L'._) = true
||  islml _ = false
-- convert an identifier to suit the assembler
and asmid i = 
    if Curry then
	case i in
	    mkid _ (c1.c2._) _ (Orignames _ _ (ss as (mi as MI m,n))) & ([c1;c2] ~= "DD" & [c1;c2] ~= "VV" & [c1;c2] ~= "MM") : 
--		if FlatNames | inprelude mi then transid n else mix (map transid [m;n]) "$_"
            let ns = if islml m | specialname n then [n] else
	        case forceprefix in
		   None : if FlatNames then [n] else if inprelude ss then [preludename; n] else [m; n]
                || Some s : [s; n]
                end
            in  mix (map transid ns) "$_"
	||  i : /*trace ("No original name for "@idtostr i)*/ (transid (idtostr i)) --fail ("No original name for "@idtostr i)
        end
    else
        let n = idtostr i in
        case forceprefix in
           Some s & (~specialname n) : mix (map transid [s; n]) "$_"
        || _ : transid n
        end
and
    prid' ('_'.s) & (~Fullname) = s
||  prid' (_.'#'.s) & (Curry) = rept (stoi s - 1) ','
||  prid' (_."Prelude.()") = "()"
||  prid' (_."Prelude.[]") = "[]"
||  prid' ('P'.'P'.'r'.'e'.'l'.'u'.'d'.'e'.'.'.'#'.s) & (Curry) = rept (stoi s - 1) ','
||  prid' "P->" = "->"
||  prid' "PPrelude.->" = "->"
||  prid' s = s
and
    -- convert an identifier for outputting
    prid i = 
    if Debug then pprid i else
    case i in
       mkids s : prid' s
    || mkidi s _ _ _ : prid' s
    || mkid n s i _ : prid' s
    end
-- In Haskell being an operator isn't enough, it must not be an alphanumeric operator
and isiso c = chr 192 <= c & c <= chr 255 & c ~= chr 215 & c ~= chr 247
and optparen s =
--	let c = hd s in
	let c = last s in
	if isalnum c | isiso c | c = '\'' | c = '_' | s = "()" | s = "[]" then
	    s
	else
	    "("@s@")"
and oprid i =
    if Debug then 
	pprid i
    else
	let s = prid' (idtostr i) in
        if (s = "") then
	    "_"
	else
	    if Curry then
		optparen s
	    else
		if id_fixity i = Nofixity then
		    s
		else
		    "("@s@")"
and
    pprid (mkids s) = s
 || pprid (mkidi s None _ _) = s@"{?}"
 || pprid (ii as mkidi s (Some (MI m,x)) _ ps) = 
	if Debug then dprid ii
	else s@"{"@m@"."@x@"}"@(if Test then show_list show_string ps else "")
 || pprid (ii as mkid n s i on) =
	if Debug then dprid ii
	else s   -- Often gives recursive data loop @"{"@proname on@"}"
and nprid (mkidi s (Some (MI m,x)) _ _) = tl m@"."@tl x
and
    dprid (mkids s) = s
 || dprid (mkidi s None _ _) = s@"{?}"
 || dprid (mkidi s (Some (MI m,x)) _ ps) = s@"{"@m@"."@x@"}"@show_list (\x.x) ps
 || dprid (mkid n s i on) =
        if IdDebug then
		s@"{"@itos n@","@case i in
			    idi_udef : "udef"
			 || idi_var v ot osel : "var{"@prvar v@protype ot@"}"@prosel osel
			 || idi_constr _ _ _ n _ _ _ : "constr-"@itos n
			 || idi_type _ t _ _ ts _ : "type"@show_list (clname o snd) ts
			 || idi_view ot _ (idi_type _ t _ _ ts _) : "view"@show_list (clname o snd) ts
			 || idi_syn _ _ _ _ : "syn"
			 || idi_class c : "class "@prclsi c
			 || idi_method ns _ c : "method-"@show_list show_int ns@"-"@prclsi c
			 || idi_inst t _ b : "inst$"@show_bool b
			 || idi_module _ : "module"
			 end @ "}" @ (if PrOrignames then "="@proname on else "")
	else
    		s@"{"@itos n@"}" @ (if PrOrignames then "="@proname on else "")

and prosel None = ""
||  prosel (Some i) = "{sel-"@idtostr i@"}"
and
    prvar (var_unknown) = "unknown"
 || prvar (var_local n) = "local "@itos n
 || prvar (var_global f) = "global "@prfinfo f "_"
 || prvar (var_pre f) = "pre"
 || prvar (var_dict _) = "dict"
and prvis Vimported = "import"
 || prvis Vexported = "export"
 || prvis Vprivate = "local"
and prfix (Infix n) = "infix-"@itos n
||  prfix (InfixL n) = "infixl-"@itos n
||  prfix (InfixR n) = "infixr-"@itos n
||  prfix _ = "_"
and protype Onotype = ""
 || protype (Ohastype t _ ss) = " "@prttype t@prspecs ss
and prspecs None = ""
 || prspecs (Some its) = "{# SPEC "@mix (map (\ (i,t,_).idtostr i@":"@prttype t) its) ", " @ " #}"
and proname Noorigname = "?Noorigname?"
 || proname (Orignames v f (m,s)) = prvis v@" "@prfix f@show_list (\s.s) [getminame m; s]
and prclsi (clsi _ t iits xs its _) = show_list (\(_,i,_).idtostr i) iits@","@show_list (show_pair (idtostr,show_list show_int)) xs@","@show_list (idtostr o tyname o snd) its
and clname (mkidecl _ (c as mkid _ _ (idi_class cl) _) _ _) = idtostr c@(if PrMore then "<"@prclsi cl@">" else "")
||  clname (mkidecl _ c _ _) = "****"@idtostr c@"****"
and tyname (mkidecl _ _ [mktcons ti _] _) = ti -- ASSERT
||  tyname _ = mkids "a"

and
    eqid (mkids s1) (mkids s2) = s1 = s2
 || eqid (mkidi s1 o1 _ _) (mkidi s2 o2 _ _) = s1 = s2 & o1 = o2
 || eqid (mkid n1 _ _ _) (mkid n2 _ _ _) = n1 = n2
 || eqid i1 i2 = fail ("No match in eqid "@dprid i1@" = "@dprid i2)
 --|| eqid i1 i2 = false -- !!!
and
    ltid (mkids s1) (mkids s2) = s1 < s2
 || ltid (mkidi s1 o1 _ _) (mkidi s2 o2 _ _) = o1 < o2
 || ltid (mkid n1 _ _ _) (mkid n2 _ _ _) = n1 < n2
and
    arity_of_id (mkid _ _ (idi_var (var_global f) _ _) _) = arity_of_finfo f
 || arity_of_id (mkid _ _ (idi_var (var_pre f) _ _) _) = arity_of_finfo f
 || arity_of_id _ = -1
and
    framesize_of_id (mkid _ _ (idi_var (var_global f) _ _) _) = framesize_of_finfo f
 || framesize_of_id _ = -1
and
    entries_of_id (mkid _ _ (idi_var (var_global f) _ _) _) = entries_of_finfo f
||  entries_of_id _ = []
and
    id_is_global (mkid _ _ (idi_var (var_global _) _ _) _) = true
 || id_is_global _ = false
and
    id_is_visible (mkid _ _ _ (Orignames Vexported _ _)) = true
||  id_is_visible (mkid _ _ _ (Orignames Vimported _ _)) = true
||  id_is_visible _ = false
and
    id_is_predef (mkid _ _ (idi_var (var_pre _) _ _) _) = true
||  id_is_predef _ = false
and
    specialpre (mkid m _ _ _) = m = Fseq | m = Fleftpat
and
    id_no (mkid m _ _ _) = m
and 
    type_of_id (mkid _ _ (idi_var _ ot _) _) = ot
||  type_of_id (mkid _ _ (idi_method ns _ (clsi _ ct iits _ _ _)) _) = 
        let (d,m,t) = select (last ns+1) iits in 
        Ohastype t (getTvars t) None
||  type_of_id (mkid _ _ (idi_constr t0 _ tbs _ _ _ _) _) =
        let t = reduce Tarr (tpart t0) (map fst3 tbs) in
	let vs = reduce union [] (map (getTvars o fst3) tbs) in
        let t' = case filter (\ (mkassert c xs).all (\v.mem v vs) xs) (cpart t0) in [] : t || k' : mktcontype k' t end in
        Ohastype t' (getTvars t') None
||  type_of_id _ = Onotype
and id_orignames (mkid _ _ _ Noorigname) = (MI "","")
||  id_orignames (mkid _ _ _ (Orignames _ _ on)) = on
||  id_orignames (mkidi _ (Some on) _ _) = on
||  id_orignames (mkidi s None _ _) = (MI "", s)  -- These last two entries should not be needed, but id_orignames is
||  id_orignames (mkids s) = (MI "", s)           -- called with a bad id somewhere.
and id_fixity (mkid _ _ _ (Orignames _ f _)) = f
||  id_fixity _ = Nofixity
and id_visibility (mkid _ _ _ (Orignames v _ _)) = v
||  id_visibility _ = Vprivate
and id_isvar (mkid _ _ (idi_var _ _ _) _) = true
||  id_isvar _ = false
and id_ismethod (mkid _ _ (idi_method _ _ _) _) = true
||  id_ismethod _ = false
and id_metsel (mkid _ _(idi_method k _ _) _) = k
and id_metarity (mkid _ _ (idi_method _ k _) _) = k
and mknewid s n = mkid n (s@itos n) idi_varu noorigname
and mknewids s n t = mkid n (s@itos n@t) idi_varu noorigname
and id_isclass (mkid _ _ (idi_class _) _) = true
||  id_isclass _ = false
and id_isinst (mkid _ _ (idi_inst _ _ _) _) = true
||  id_isinst _ = false
and id_issyn (mkid _ _ (idi_syn _ _ _ _) _) = true
||  id_issyn _ = false
and id_isconstr (mkid _ _ (idi_constr _ _ _ _ _ _ _) _) = true
||  id_isconstr _ = false
and updvis vi (mkid n s v (Orignames _ f on)) = mkid n s v (Orignames vi f on)
||  updvis _ i = i
and updidname (mkid n _ i on) s = mkid n s i on

and get_id_kind (mkid _ _ (idi_type k _ _ _ _ _) _) = k
||  get_id_kind (mkid _ _ (idi_view _ _ (idi_type k _ _ _ _ _)) _) = k
||  get_id_kind (mkid _ _ (idi_conctype (mktcons _ ts) _) _) = mkkarrows (map (\_.mkkground) ts) mkkground
||  get_id_kind (mkid _ _ (idi_syn k _ _ _) _) = k
||  get_id_kind (mkid _ _ (idi_class (clsi k _ _ _ _ _)) _) = k
||  get_id_kind i = mkkground --fail ("No match in get_id_kind: "@dprid i)

and getminame (MI s) = s

and tupstr n = 'P'.'#'.itos n

and getspec (mkid _ _ (idi_var _ (Ohastype _ _ ots) _) _) = ots
||  getspec _ = None

and mkqids m (i as (c.cs)) = if QualifiedNames & c='_' then mkids (c.m@('.'.cs)) else mkids i -- XXX
and mkpids i = mkqids "Prelude" i

and dropqual (s as (c.ss)) & (QualifiedNames & isuppm ss) =
	case splitat '.' ss in
	   (_, "") : s
	|| (_, i) : c.i
	end
||  dropqual s = s
and isuppm ('_'.cs) = isuppm cs
||  isuppm (c._) = isupper c
||  isuppm _ = false

end

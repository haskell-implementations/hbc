module -- cutil
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/ttype_t.t"
#include "../expr/einfo_t.t"
#include "../expr/types_t.t"
#include "../expr/id.t"
#include "../expr/ttype.t"
#include "../expr/tinfo.t"
#include "../expr/error.t"
#include "../expr/constrfun.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "../rename/renameutil.t" /* hasext */
#include "hexpr.t"
#include "misc.t"
export const, idtoconstr, CTuple, CXTuple, convstr, chkpat, mkint, mkchar;
rec
    const s t it n = mkconstr (Cconstr s t it n (false,[],[]) []) []
and mkint i = const (itos i) Tint ITint i
and mkchar c = const ['\''; c; '\''] Tchar ITchar (ord c)
and idtoconstr (mkid no s (idi_constr t e tbl n atl _ vw) _) =
    case tpart t in
       mktcons (mkid _ _ (idi_type _ _ _ ti _ _) _) _ :
	let (mktinfo a b c d ee f g _) = ti in
	let ti' = mktinfo a b c d ee f g vw -- XXX
	in  Cconstr s t ti' n e [(t,b) ;; (t,b,s) <- tbl ]
    || _ : fail ("idtoconstr " @ prttype t)
    end
and CXTuple b n =
        let rec bs = b.bs in
        Cconstr (tupstr n) (Ttuple n) (ITtuple n) 0 (false,[],[]) (combine (tvars n, bs))
and CTuple n = CXTuple false n
and convstr' [] = []
||  convstr' ('\\'.'t' .r) = chr 9.convstr' r
||  convstr' ('\\'.'0' .r) = chr 0.convstr' r
||  convstr' ('\\'.'u' .d1.d2.d3.d4.r) = chr (fromhex [d1;d2;d3;d4] 0) . convstr' r
||  convstr' ('\\'.'\\'.r) = '\\' .convstr' r
||  convstr' (      c  .r) = c    .convstr' r
and convstr s =
    let s' = convstr' s in
    if exists (\c.ord c > 255) s' then
	let nil = mkconstr hcnil []
	and cons x xs = mkconstr hccons [x; xs]
	in  reduce (\c.\l.cons (mkchar c) l) nil s'
    else mkconstr (mkstring s') []
and fromhex "" a = a
||  fromhex (c.cs) a = fromhex cs (a * 16 + fromhex1 c)
and fromhex1 c = if isdigit c then ord c - ord '0' else ord c - ord 'a' + 10
and chkdup = anysameeq eqid o (filter (\i.~Curry | H1_3 | idtostr i ~= "_+")) o gids -- I hate n+k
and chlex [] = false 
||  chlex ((mklt _).llex) = chlex llex
||  chlex ((mkltint _).llex) = chlex llex
||  chlex ((mkltid _).llex) = chlex llex
||  chlex ((mkltsym _).llex) = chlex llex
||  chlex ((mkunq ex).llex) = chkp ex | chlex llex
and chkp (mkas i e) = chkp e
||  chkp (mkbrack _ llex) = chlex llex
||  chkp (mkcondp p c) = chkp p
||  chkp (mklazyp p) = chkp p
||  chkp (mkconstr c es) = exists chkp es
||  chkp (mkident i) = false
||  chkp (mkinfo _ e) = chkp e
||  chkp (mkap (mkap (mkident plus) (mkident _)) (mkconstr c [])) = ~(Curry & NPlusK & eqid plus hiadd & isinteger c)	-- All this trouble just for n+1 patterns!
||  chkp _ = true
and chkpat e =
	if chkp e then 
	    case leftmost e in
		mkident i : merror e ("[72] Variable "@oprid i@" leftmost in pattern")
            ||  (e as mkerror msg) : e
	    ||  _ : merror e "[74] Bad pattern"
            end
	else if chkdup e then merror e "[73] Duplicate id in pattern"
	else e
end

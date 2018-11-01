module -- eqtrans
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/constr_t.t"
#include "../expr/einfo_t.t"
#include "../transform/hexpr.t"
#include "Expr_t.t"
#include "../misc/misc.t"
#include "../misc/flags.t"
#include "Eprint.t"

export addrestr;
rec
    addrestr pids e u = S pids true e u
and cmpop m = member eqid m [hieq; hine; hile; hige; hilt; higt]
and res e u = Einfo (trestr (mktvar u)) e
and
    S pids b (Emodule i expl ds) u = 
	Uap (Emodule i expl) (Umap (Umapsnd (S pids b)) ds u)
||  S pids b (Elam i e) u = Uap (Elam i) (S pids true e u)
||  S pids b (Eap (Eap (e as Evar i) e1) e2) u & (~Curry & cmpop i) =
	let (e1', u') = S pids false e1 (u+1) in
	let (e2', u'') = S pids false e2 u' in
	(Eap (Eap e (res e1' u)) e2', u'')
||  S pids b (Eap e1 e2) u =
	let (e1', u') = S pids false e1 u in
	Uap (Eap e1') (S pids false e2 u')
||  S pids b (e as Evar i) u =
	if b | Specialize & (getspec i ~= None | member eqid i pids) then
	    (res e u, u+1)
	else
	    (e, u)
||  S pids b (Ecase e pl dp) u =
	let (e', u') = S pids false e u in
	let (pl', u'') = Umapthd (S pids true) pl u' in
	let (dp', u''') = S pids true dp u'' in
   	(Ecase e' pl' dp', u''')
||  S pids b (Elet r dl e) u =
	let (dl', u') = Umapsnd (S pids false) dl u in
	Uap (Elet r dl') (S pids b e u')
||  S pids b (Econstr c el) u = Uap (Econstr c) (Umap (S pids false) el u)
||  S pids b (e as Efailmatch _) u = (e, u)
||  S pids b (e as Ecfunction _ _) u = (e, u)
||  S pids b (e as Einfo notchk _) u = (e, u)
||  S pids b (Einfo f e) u = Uap (Einfo f) (S pids b e u)
||  S pids b e u = fail ("addrestr "@pr e)
end

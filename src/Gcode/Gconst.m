module
#include "../expr/id.t"
#include "../expr/einfo_t.t"
#include "../misc/flags.t"
#include "../expr/constrfun.t"
#include "../ExprE/Expr_t.t"
#include "Gcodedef_t.t"
#include "Genv.t"
#include "Gutil.t"
#include "Gid.t"
#include "Gprint.t"
#include "../funnos.h"

export Gconst, isconstant;
rec isconstant _ (Eidapl _ []) = false
||  isconstant _ (Econstr _ []) = false
--||  isconstant r (Einfo t e) & (~mem t [strict; metcall; vecreg2; vectordef]) = isconstant r e
||  isconstant r e = isconst r e
and isconst r (Eidapl i es) = id_no i < PBSELBASE & ~id_is_predef i & lookenv r i = 0 & all (isconst r) es
||  isconst r (Econstr c es) = all (isconst r) es
--||  isconst r (Einfo t e) & (~mem t [strict; metcall; vecreg2; vectordef]) = isconst r e
||  isconst r _ = false

and Gconst (Eidapl i []) = [PUSHGLOBAL i]
||  Gconst (Eidapl i es) =
	let lel = length es in
	let ar = gnoargs i in
	if ar ~= -1 & ar ~= 0 & ar <= lel & ~NoVector then
		prev es @
		[ MKAPLV i ar ] @
		rept (lel-ar) (MKAP (idtopstr i))
	else
		prev (Eidapl i [].es) @
		rept lel ((if ar > 0 & lel < ar then MKCAP else MKAP) (idtopstr i))
||  Gconst (Econstr c es) =
	prev es @
	[ CONSTR (cname c, typenamefromtinfo (ctinfo c)) (constrtype c) (cno c) (length es)]
||  Gconst (Einfo _ e) = Gconst e

and prev es = concmap Gconst (reverse es)

end

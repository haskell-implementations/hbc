module
#include "../expr/id.t"
#include "../expr/constr_t.t"
#include "../expr/types_t.t"
#include "../expr/ttype.t"
#include "../expr/tinfo.t"
#include "hexpr.t"
#include "cutil.t"

export newid, enil, econs, elist, echrcons, eeq, xcchar, newids, newid;
rec xcchar c = Cconstr ['\''; c; '\''] Tchar ITchar (ord c) (false,[],[]) []
and cchr c = const ['\''; c; '\''] Tchar ITchar (ord c)
and enil = mkconstr hcnil []
and econs c l = mkconstr hccons [c; l]
and echrcons c l = econs (cchr c) l
and elist l = reduce econs enil l
and eeq = mkident hieq
and newid i = mkident (mknewid "I" i)
and newids s i = mkident (mknewids "I" i s)
end

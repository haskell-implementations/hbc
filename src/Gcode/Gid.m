module
#include "../expr/types_t.t"
#include "../expr/id_t.t"
#include "../expr/ttype_t.t"
#include "../expr/booltree.t"
#include "../expr/ttype.t"
#include "../expr/einfo_t.t"
#include "../expr/id.t"
#include "../funnos.h"

export identityfun, pbselfun;

rec identityfun = mkid Fid "Pid" (idi_var idinfo idtype None) idorig
and idinfo = var_global (finfo 1 [] (btands[btors[btvar 0]],btff) 2 None)
and idtype = Ohastype (Tarr (Tvar 1) (Tvar 1)) [1] None
and idorig = Orignames Vimported Nofixity (MI preludeBuiltin, "Pid")

and pbselfun = mkid Fpbsel "Ppbsel" (idi_var pbselinfo pbseltype None) pbselorig
and pbselinfo = var_global (finfo 1 [] (btands[btors[btvar 0]],btff) 2 None)
and pbseltype = Ohastype (Tarr (Tvar 1) (Tvar 2)) [1; 2] None
and pbselorig = Orignames Vimported Nofixity (MI preludeBuiltin, "Ppbsel")

end

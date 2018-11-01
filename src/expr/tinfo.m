module -- tinfo
#include "types_t.t"
#include "constr_t.t"
#include "id_t.t"
#include "ttype_t.t"
#include "ttype.t"
#include "../transform/hexpr.t"
#include "../misc/flags.t"

export ITint, ITbool, ITchar, ITlist, ITtuple,
	get_type_id_from_tinfo, get_no_of_constr_from_tinfo,
	get_component_types_from_tinfo, get_cs_from_tinfo, get_has_existential,
        get_isotype, get_view;

    ITint = gettinfo hiInt
and ITchar = gettinfo hiChar
and ITbool = gettinfo hiBool
and ITlist = gettinfo hiList
and ITtuple n = mktinfo (Ttuple n) 1 false false [] false false None

and get_type_id_from_tinfo (mktinfo (mktcons id _) _ _ _ _ _ _ _) = id
and get_no_of_constr_from_tinfo (mktinfo _ no_constr _ _ _ _ _ _) = no_constr
and get_cs_from_tinfo (mktinfo _ _ _ _ cs _ _ _) = cs
and get_component_types_from_tinfo (mktinfo _ _ _ _ comp_types _ _ _) = comp_types
and get_has_existential (mktinfo _ _ _ _ _ has_ext _ _) = has_ext
and get_isotype (mktinfo _ _ _ _ _ _ iso _) = iso
and get_view (mktinfo _ _ _ _ _ _ _ v) = v

end

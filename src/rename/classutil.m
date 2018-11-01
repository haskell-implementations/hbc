module
#include "../misc/flags.t"
#include "../expr/types_t.t"
#include "../expr/constr_t.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/einfo_t.t"
#include "../expr/idtab.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/impexp_t.t"
#include "../expr/tinfo.t"
#include "../expr/pprint.t"
#include "../misc/util.t"
#include "../misc/misc.t"
#include "../type/conutil.t"			/* subsume */
#include "renenv.t"
export sortcon, gettypeinsts, getclassinfo;
rec 
    -- sort a context into its canonical order
    sortcon k = 
        case partition isaerror k in
	   (errs, aas) : errs @ sort (\x.\y.aname x < aname y) (mkseteq (\x.\y.aname x = aname y) aas)
	end

and aname (mkassert x _) = snd (id_orignames x)
--||  aname (mkaerror msg) = fail ("sortcon-aname "@msg)
and isaerror (mkassert _ _) = false
||  isaerror _ = true
and gtype2inst env = let (_, ti, _) = rgetct env in ti
and gclass2inst env = let (ci, _, _) = rgetct env in ci
and gsupers env = let (_,_,s) = rgetct env in s
and gettypeinsts env i = /*trace ("gettypeinsts "@idtostr i)*/( ass i (gtype2inst env))
and getclassinfo k env (cl as mkcdecl aas a) iits = 
        let (mkassert cc _) = a in
--trace ("getclassinfo "@ prttype (cdecl2type cl)) 
--trace ("getclassinfo "@ show_list xxx iits)
(
	let ss = ass cc (gsupers env) in
	let insts = ass cc (gclass2inst env) in
	clsi k (mkcdecl aas a) iits ss insts (length (filter (\(_,is).length is=1) ss))
)
and ass i al = assocdefeq eqid i al []

--and xxx (i1, i2, t) = "("@prid i1@","@prid i2@","@prttype t@")"
end

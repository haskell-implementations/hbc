module -- multi
--
-- eliminate multibindings
--
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/constr_t.t"
#include "../expr/ttype_t.t"
#include "../expr/einfo_t.t"
#include "../expr/types_t.t"
#include "../expr/ttype.t"
#include "../expr/pprint.t"
#include "../expr/impexp_t.t"
#include "../expr/booltree.t"
#include "../funnos.h"
#include "../transform/misc.t"
#include "../misc/flags.t"
#define MAXSEL 6
-- Should read this from data base.
#define SELSIZE(m) 2
export pselect, selids, pbselect;
rec
    selno  n m = FSELBASE + m*(m+1)/2 + n
and selstr n m = "Ps"@itos m@"_"@itos (n+1)
and selid f t n m = 
    let s = selstr n m in
    mkid (selno n m) s (idi_var (var_global f) (Ohastype t (count 1 m) None) None) (Orignames Vimported Nofixity (MI preludeBuiltin, s))
and seltype m n = Tarr (Ttuple m) (Tvar (n+1))
and selfinfo m n = finfo 1 [] (btands[btors[btvar 0]],btff) (SELSIZE(m)) None
and seliid m n = 
        let t = seltype m n
	and f = selfinfo m n in
	selid f t n m
and selids = conc (for 2 MAXSEL (\n. for 0 (n-1) (seliid n)))
and presel = if PreTupleSel then for 2 MAXSEL tupstr else []
and selector n m = mkident (selid (selfinfo m n) (seltype m n) n m)
and findi n i (mkident i1.is) = if eqid i i1 then n else findi (n+1) i is
and pselect e (i as mkident ii) p =
	case p in
	   (mkconstr (Cconstr s _ _ _ _ _) is) & (mem s presel & all isI is) :
		mkap (selector (findi 0 ii is) (length is)) e
	||  _ : mkcase e [(p, i)]
	end
and pbselstr n m = "PBs"@itos m@"_"@itos (n+1)
and pbselno n m = PBSELBASE + m*(m+1)/2 + n
and pbselid f t n m =
        let s = pbselstr n m in
	mkid (pbselno n m) s (idi_var (var_global f) (Ohastype t (count 1 m) None) None) (Orignames Vimported Nofixity (MI preludeBuiltin, s))
and pbselector n m = mkident (pbselid (selfinfo m n) (seltype m n) n m)
and pbselect e n m = mkap (pbselector n m) e
end

module
#include "../misc/triple.t"
#include "impexp_t.t"
#include "id.t"
#include "ttype_t.t"
#include "ttype.t"
#include "pprint.t"
#include "types_t.t"
export expid, prexpid, primpid, impids;
rec expid (mkexpid i) = i
||  expid (mkexpidall i) = i
||  expid (mkexpidsome i _) = i
||  expid (mkexpidmodall i) = i
and prexpid (mkexpid i) = oprid i
 || prexpid (mkexpidall i) = oprid i @ "(..)"
 || prexpid (mkexpidsome i is) = oprid i @ "(" @ mix (map oprid is) "," @ ")"
 || prexpid (mkexpidmodall i) = oprid i @ ".."
and primpid (mkimpid i t _ _) = oprid i @ ": " @ prttype t
 || primpid (mkimptype _ t (n,f)) = "type " @ prttype t @(if n > 0 then "{"@itos n@","@show_bool f@"}" else "")
 || primpid (mkimpeqtype _ t ats od iso _) = (if iso then "newtype " else "type ") @ prttype t @ " = " @ mix (map (\(mkcons ci _ tbs _).prid ci @ show_list (prttype o fst3) tbs) ats) " + " @ prderiv od
 || primpid (mkimpsyn _ s d) = prttype s @ " == " @ prttype d
 || primpid (mkimpclass _ t b _) = "class " @ prcdecl t @ " where " @ prdefg 4 b @ "\nendclass"
 || primpid (mkimpinstance t _ _) = "instance " @ pridecl t
 || primpid (mkimpimport id imps rens) = "import "@oprid id@" "@show_list prexpid imps@" "@show_list (show_pair (prid,prid)) rens
 || primpid (mkimpids is t _) = mix (map oprid is) "," @ ": " @ prttype t
 || primpid (mkimpview _ t tof ats) = " view "@prttype t@" of "@prttype tof @ " = " @ mix (map (\(mkcons ci _ tbs _).prid ci @ show_list (prttype o fst3) tbs) ats) " + "
 || primpid (mkimpctype t prods) = (" conctype " @ prttype t @ " = " @ mix (map prqdef prods) " + "
					                                         where rec prqdef l = prprod 0 l)

and impids (mkimpid i _ _ _) = [i]
 || impids (mkimptype _ t _) = tdef t
 || impids (mkimpeqtype _ t _ _ _ _) = tdef t
 || impids (mkimpsyn _ s _) = tdef s
 || impids (mkimpclass _ t _ _) = cdef t
 || impids (mkimpinstance t _ _) = []
 || impids (mkimpimport _ _ _) = []
 || impids (mkimpids is _ _) = is
 || impids (mkimpview _ t _ _) = tdef t
and tdef (mktcontype ts t) = tdef t
||  tdef (mktcons i _) = [i]
and cdef (mkcdecl _ (mkassert i _)) = [i]
end

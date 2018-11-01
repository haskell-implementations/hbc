module -- traceprint
--
-- $Header: traceprint.m 
--
-- some traceprints for check.m 
--
#include "../expr/constr_t.t"
#include "../expr/einfo_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/types_t.t"
#include "../expr/eq.t"
#include "../expr/id.t"
#include "../expr/id_t.t"
#include "../expr/pprint.t"
#include "../expr/constrfun.t"
#include "../expr/exprid.t"
#include "../transform/misc.t"
#include "../type/subst_t.t"
%#include "../type/prefix.t"
%#include "../type/subst.t"
%#include "xsubst.t"
%#include "../type/unify.t"


export prcaseposs, prposs; 

rec prcaseposs eTtts =
     conc (map2 (\(e,T,t1,t2).\n. 
                    "Nr "@showint n@"\nExp: "@concmap ppr e@
                                    "\nSubst: "@ prTR T@
                                    "\nPat-type: "@prttype t1@
                                    "\nExp-type: "@prttype t2@"\n\n"
                )
                eTtts
                (from 1)
          )

and prposs eTts = 
      conc (map2 (\(e,T,t).\n. 
                    "Nr "@showint n@"\nExp: "@concmap ppr e@
                                    "\nSubst: "@ prTR T@
                                    "\nType: "@prttype t@
                                    "\n\n"
                 )
                 eTts
                 (from 1)
           )
end


module
#include "types_t.t"

export idbind,idexpr;

rec idbind b =
--  idbind : Binding -> List Id
	case b
	in mkbtype  _ _ _ _		: []
	|| mkbctype _ _			: []
	|| mkbpat   pes			: concmap (idexpr o fst) pes
--	|| mkbpat   [(mkident i, _)] 	: [i]
	|| mkbmulti pt _		: idexpr pt
	|| mkband   b1 b2		: idbind b1 @ idbind b2
	|| mkbrec   b			: idbind b
	|| mkblocal _ b			: idbind b
	|| mkbnull			: []
	|| mkbpragma _			: []
	end

and idexpr e =
--  idexpr : Texpr -> List Id
	case e
        in mkap e1 e2                   : idexpr e1 @ idexpr e2
        || mklam i e                    : idexpr e --?!! i
        || mkcase e pes                 : idexpr e @ 
					    concmap (\(p,e).idexpr p @ idexpr e)
						    pes
        || mkletv b e                   : idbind b @ idexpr e
        || mkident  i 			: [i]
	|| mkmodule _ _ _ _ b           : idbind b
	|| mkconst _			: []
	|| mkcfunction _ _              : []
	|| mkerror _			: []
	|| mkas     i  e  		: i.idexpr e
	|| mkcondp  pt _		: idexpr pt
	|| mklazyp e			: idexpr e
	|| mkconstr _  es 		: concmap idexpr es 
	|| mkfailmatch _        	: []
	|| mkinfo _ e			: idexpr e
	|| mklistf _ es			: concmap idexpr es
	|| mklistg e _   		: idexpr e
	|| mkbrack _ lexs       	: concmap idexpr (concmap unq lexs)
	                                     where unq (mkunq e) = [e]
                                                || unq    _      = []
	end
end

module
#include "jfunutil.t"
-- remove identical code blocks
export remdup;

    -- this is (at least) n**2, but quite fast nevertheless
    remdup bs = rd bs []
    where rec rd [] ts = reverse ts
	   || rd ((b as (e,x,c)).bs) ts =
		case dupno b ts in
		   0 : rd bs (b.ts)
		|| n : rd bs ((e,n,[]).ts)
		end
    and dupno (_,x,c) [] = 0
     || dupno (b as (_,x1,c1)) ((e,x2,c2).ts) =
		if x1=x2 & eqGs c1 c2 then e else dupno b ts
end

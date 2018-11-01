-- Manipulating locations
-- Phil Wadler
--  2 Nov 86:  Created.

module
export locations, show_loc;
rec
   locations xs		=  locs (1,1) xs
and
   locs lc []		=  [(lc,(chr 0))]
|| locs lc (x.xs)	=  (lc,x).locs (nextloc lc x) xs
and
   nextloc (l,c) x	=  if x = '\n' then (l+1,1)
				       else (l,c+1)
and
   show_loc (l,c)	=  "line " @ show_int l @ ", char " @ show_int c
end

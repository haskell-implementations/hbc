module
#include "pptype.t"
#include "ppparse.t"
#include "parse.t"
#include "location.t"
export  ppshow, pp;
rec
   ppshow n i  =  pptree n i o parse ppparse o locations
and
   pp us       =  [ppopen] @ us @ [ppclose]
and
   pptree n i (PPnode x xts)  =  let us = ppflat (PPnode x xts)  in
				   if  fits (n-i) us  then
				     us
				   else
				     let  j = i + length x  in
				       x @ mix (map (pptree n j) xts)
					       ("\n" @ space j)
and
   ppflat (PPnode x xts)  =  x @ concmap ppflat xts
and
   fits i []      =  true
|| fits i (x.xs)  =  i > 0  &  fits (i-1) xs
end

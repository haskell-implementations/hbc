module
#include "pptype.t"
export  ppadd;
rec
   ppadd (PPnode x []) y  	=  PPnode (x@y) []
|| ppadd (PPnode x (xt.xts)) y	=  let  (zts,zt) = snocize xt xts in
				     PPnode x (zts @ [ppadd zt y])
and
   snocize x []			=  ([], x)
|| snocize x (y.ys)		=  ((x.zs, z)  where  (zs,z) = snocize y ys)
end

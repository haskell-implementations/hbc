module
export VV_Eq_a, VV_Ord_a;
rec VV_Eq_a _n _d = 
#if 1
	(
	case (_n{#NOEVAL#}){#LIMITOK#} in
	   0 : ((\_x.\_y._x=_y){#METCALL#}){#VECREG2#}
	|| 1 : ((\_x.\_y._x~=_y){#METCALL#}){#VECREG2#}
	end
	){#VECTORDEF#}
#else
	Pfail "VV_Eq_a"
#endif

and VV_Ord_a _n _d = 
#if 1
        (
	case (_n{#NOEVAL#}){#LIMITOK#} in
	   0 : (VV_Eq_a, []){#VECREG2#}
	|| 1 : ((\_x.\_y. _x < _y){#METCALL#}){#VECREG2#}
	|| 2 : ((\_x.\_y. _x <= _y){#METCALL#}){#VECREG2#}
	|| 3 : ((\_x.\_y. _x >= _y){#METCALL#}){#VECREG2#}
	|| 4 : ((\_x.\_y. _x > _y){#METCALL#}){#VECREG2#}
	|| 5 : ((\_x.\_y. if _x > _y then _x else _y){#METCALL#}){#VECREG2#}
	|| 6 : ((\_x.\_y. if _x > _y then _y else _x){#METCALL#}){#VECREG2#}
	|| 7 : ((\_x.\_y. _x = _y){#METCALL#}){#VECREG2#}
	|| 8 : ((\_x.\_y. _x ~= _y){#METCALL#}){#VECREG2#}
	end
	){#VECTORDEF#}
#else
	Pfail "VV_Ord_a"
#endif
end

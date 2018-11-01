/*
**	Pfrom:		generate lists
**
*/
module
export Pfrom, Pfromby, Pfromto, Pfrombyto, Psel, Praise;
rec Pfrom _n = _n . Pfrom (_n + 1)
and Pfromto _m _n = Pfromtop _m _n
--and Pfromto _m _n = if _n < _m then Pfromton _m _n else Pfromtop _m _n
--and Pfromton _m _n = if _m < _n then [] else _m . Pfromton (_m - 1) _n
and Pfromtop _m _n = if _m > _n then [] else _m . Pfromtop (_m + 1) _n

and Pfrombyto _m _k _n = if _k < 0 then Pfrombyton _m _k _n else Pfrombytop _m _k _n
and Pfrombyton _m _k _n = if _m < _n then [] else _m . Pfrombyton (_m + _k) _k _n
and Pfrombytop _m _k _n = if _m > _n then [] else _m . Pfrombytop (_m + _k) _k _n
and Pfromby _n _k = _n . Pfromby (_n + _k) _k
and Psel _l _n = if _n < 0 then Pfail "sel < 0" else Psel1 _l _n
and Psel1 (_x._xs) 0 = _x
||  Psel1 (_x._xs) _n = Psel _xs (_n - 1)
and Praise _l _n = if _n < 0 then Pfail "raise < 0" else Praise1 _l _n
and Praise1 _n 0 = 1
||  Praise1 _n _m = let _p = Praise _n (_m / 2) in if _m % 2 = 0 then _p * _p else _n * _p * _p
end

/*
**	Ppre:	builtin functions.
*/
module
export (=), (~=), (<), (>), (<=), (>=), 
       _negate, _ord, _chr, Ptag, (|), (&), (~), (,), (o), /*(@),*/ Pseq, Pcno, (?), (?? ), (^),
       (*), (/), (%), (+), (-),
       Prindex,
       Prindexu, 
       (*.), (/.), (+.), (-.), PFloatNeg,
       (*#), (/#), (%#), (+#), (-#),
       _bitand, _bitor, _bitxor, _bitcompl, _bitlsh, _bitrsh, _bitrsha;

-- The ice is really thin here, but it works.
    (=)   _x _y = _x  = _y
and (~=)  _x _y = _x ~= _y
and (<)   _x _y = _x  < _y
and (>)   _x _y = _x  > _y
and (<=)  _x _y = _x <= _y
and (>=)  _x _y = _x >= _y
and (*)   _x _y = _x * _y
and (/)   _x _y = _x / _y
and (%)   _x _y = _x % _y
and (+)   _x _y = _x + _y
and (-)   _x _y = _x - _y
and _negate  _x = - _x
and _ord     _x = _ord _x
and _chr     _x = _chr _x
and Ptag     _x = Ptag _x
and Pcno     _x = Pcno _x
and (|)   _x _y = _x | _y
and (&)   _x _y = _x & _y
and (~)   _x    = ~ _x
and (,)   _x _y = (_x , _y)
and (o)   _f _g _x = _f (_g _x)
/*
and (rec (@)   []         _ys = _ys
     ||  (@)   (_x . _xs) _ys = _x . (_xs @ _ys))
*/
and Pseq  _x _y = Pseq _x _y

and _bitand _x _y = _bitand _x _y
and _bitor _x _y = _bitor _x _y
and _bitxor _x _y = _bitxor _x _y
and _bitcompl _x = _bitcompl _x
and _bitlsh _x _y = _bitlsh _x _y
and _bitrsh _x _y = _bitrsh _x _y
and _bitrsha _x _y = _bitrsha _x _y

and (*.)   _x _y = _x *. _y
and (/.)   _x _y = _x /. _y
and (+.)   _x _y = _x +. _y
and (-.)   _x _y = _x -. _y
and PFloatNeg _x = -. _x

and (*#)   _x _y = PIntegerMul _x _y
and (/#)   _x _y = PIntegerDiv _x _y
and (%#)   _x _y = PIntegerMod _x _y
and (+#)   _x _y = PIntegerAdd _x _y
and (-#)   _x _y = PIntegerSub _x _y

and (?? )   _x _y = Psel _x _y
and (?)    _x _y = Paindex _x _y
and (^)    _x _y = Praise _x _y

and Prindex = (Pfail "Prindex"):(_LArray *a -> _Int -> *a)
and Prindexu = (Pfail "Prindexu"):(_LArray *a -> _Int -> *a)

end

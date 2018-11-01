module
export Prange, Pindex, PinRange;
rec
    Prange (_l, _h) = _fromto (_ord _l) (_ord _h)
and _fromto _l _h = if _l > _h then [] else Ptag _l . _fromto (_l + 1) _h
and Pindex (_l, _h) _i = _ord _i - _ord _l
and PinRange (_l, _h) _i = _ord _l <= _ord _i & _ord _i <= _ord _h
end

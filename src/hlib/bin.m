module
export PshowBin, PreadBin, PnullBin, PisNullBin;
rec PshowBin _x _xs = _x._xs
and PnullBin = []
and PreadBin (_x._xs) = (_x, _xs)
and PisNullBin [] = _true
||  PisNullBin _ = _false
end

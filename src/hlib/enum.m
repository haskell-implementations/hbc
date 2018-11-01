module -- built in enumeration
export PenumFT, PenumFTT, PenumFTTU;
rec PenumFT _x _y = _up1 ((_ord _x){#STRICT#}) ((_ord _y){#STRICT#})
and PenumFTT _x _x1 _y = 
        if _ord _x < _ord _x1 then 
	    _upn   (_ord _x1 - _ord _x) (_ord _x) ((_ord _y){#STRICT#})
	else 
	    _downn (_ord _x1 - _ord _x) (_ord _x) ((_ord _y){#STRICT#})
and PenumFTTU _x _x1 _lo _hi = 
        if _ord _x < _ord _x1 then 
	    _upn   (_ord _x1 - _ord _x) (_ord _x) ((_ord _hi){#STRICT#})
	else 
	    _downn (_ord _x1 - _ord _x) (_ord _x) ((_ord _lo){#STRICT#})
and _up1      _x _y = if (_x{#NOEVAL#}) > (_y{#NOEVAL#}) then [] else Ptag (_x{#NOEVAL#}) . _up1      ((_x{#NOEVAL#})+1)              _y
and _upn _n   _x _y = if (_x{#NOEVAL#}) > (_y{#NOEVAL#}) then [] else Ptag (_x{#NOEVAL#}) . _upn   _n ((_x{#NOEVAL#})+(_n{#NOEVAL#})) _y
and _downn _n _x _y = if (_x{#NOEVAL#}) < (_y{#NOEVAL#}) then [] else Ptag (_x{#NOEVAL#}) . _downn _n ((_x{#NOEVAL#})+(_n{#NOEVAL#})) _y
end

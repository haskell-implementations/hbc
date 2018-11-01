module
export Pbigeq, Pbigne, Pbiglt, Pbigle, Pbiggt, Pbigge, Peqi, Plti, Plei, Pord;
    Pbigeq _x _y = _x = _y
and Pbigne _x _y = _x ~= _y
and Pbiglt _x _y = _x < _y
and Pbigle _x _y = _x <= _y
and Pbiggt _x _y = _x > _y
and Pbigge _x _y = _x >= _y
and Peqi _x _y = _x = (_y:_Int)
and Plti _x _y = _x < (_y:_Int)
and Plei _x _y = _x <= (_y:_Int)
and Pord _x = _ord _x
end

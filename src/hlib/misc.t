import PinRange: ((_Char # _Char)->(_Char->_Bool)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import Pindex: ((_Char # *a)->(_Char->_Int)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import Prange: ((_Char # _Char)->(_List _Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};

import PIntegerAdd:_Integer->_Integer->_Integer {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import PIntegerSub:_Integer->_Integer->_Integer {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import PIntegerMul:_Integer->_Integer->_Integer {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import PIntegerDiv:_Integer->_Integer->_Integer {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import PIntegerMod:_Integer->_Integer->_Integer {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import PIntegerDivMod:_Integer->_Integer->(_Integer # _Integer) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import PIntegerNeg:_Integer->_Integer {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import PInt2Integer:_Int->_Integer {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import PInteger2Int:_Integer->_Int {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import PInteger2IntList:_Integer->(_List _Int) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import PInteger2String:_Int->_Integer->_String {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import PIntegerPowMod:_Integer->_Integer->_Integer->_Integer {# ARITY _ = 3 #}{# STRICTNESS _ = "0&1&2,F" ST #};
import PIntegerGcd:_Integer->_Integer->_Integer {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import PIntegerAnd:_Integer->_Integer->_Integer {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import PIntegerOr:_Integer->_Integer->_Integer {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import PIntegerSqrt:_Integer->(_Integer # _Integer) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};

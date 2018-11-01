import Parray: _Int -> _Int-> (_List *a -> *b) -> _List(_Int # *a) -> _LArray *b {# ARITY _ = 4 #}{# STRICTNESS _ = "0&1,F" ST #};
import Psarray: _Int -> _Int -> _Int -> _List(_Int # *a) -> _LArray *b {# ARITY _ = 4 #}{# STRICTNESS _ = "0&1&2&3,F" ST #};
import Pmkarray: _Int -> _Int-> *a -> _LArray *a {# ARITY _ = 3 #}{# STRICTNESS _ = "0&1,F" ST #};
import Paindex: (_LArray *a) -> _Int -> *a {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import Paupdate: (_LArray *a) -> _Int -> *a -> _Unit {# ARITY _ = 3 #}{# STRICTNESS _ = "0&1,F" ST #};
import Plowerbound: (_LArray *a) -> _Int {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import Pupperbound: (_LArray *a) -> _Int {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};

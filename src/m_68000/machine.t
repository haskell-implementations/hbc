import indreg: Addrmode {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import bigeqreg: Addrmode {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import tagreg: Addrmode {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import argcreg: Addrmode {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import use3op: Bool {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import usecase: (*a->(Int->(Int->(Int->Bool)))) {# ARITY _ = 4 #}{# STRICTNESS _ = "3,1&2&3" ST #};
import Dregs: (List Int) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import Aregs: (List Int) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import assemblercode: ((List Mcode)->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};

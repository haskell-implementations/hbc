import indreg: Addrmode {# ARITY indreg = 0 #}{# STRICTNESS indreg = "F,T" ST #};
import bigeqreg: Addrmode {# ARITY bigeqreg = 0 #}{# STRICTNESS bigeqreg = "F,T" ST #};
import tagreg: Addrmode {# ARITY tagreg = 0 #}{# STRICTNESS tagreg = "F,T" ST #};
import argcreg: Addrmode {# ARITY argcreg = 0 #}{# STRICTNESS argcreg = "F,T" ST #};
import use3op: Bool {# ARITY use3op = 0 #}{# STRICTNESS use3op = "F,T" ST #};
import usecase: (*a->(Int->(Int->(Int->Bool)))) {# ARITY usecase = 4 #}{# STRICTNESS usecase = "3,1&2&3" ST #};
import Dregs: (List Int) {# ARITY Dregs = 0 #}{# STRICTNESS Dregs = "F,F" ST #};
import Aregs: (List Int) {# ARITY Aregs = 0 #}{# STRICTNESS Aregs = "F,F" ST #};
import assemblercode: ((List Mcode)->(List Char)) {# ARITY assemblercode = 1 #}{# STRICTNESS assemblercode = "F,F" ST #};

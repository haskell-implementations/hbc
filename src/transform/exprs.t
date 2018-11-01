import newid: (Int->Texpr) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,T" ST #};
import newids: ((List Char)->(Int->Texpr)) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,T" ST #};
import xcchar: (Char->Constr) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,T" ST #};
import eeq: Texpr {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import echrcons: (Char->(Texpr->Texpr)) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,T" ST #};
import elist: ((List Texpr)->Texpr) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import econs: (Texpr->(Texpr->Texpr)) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,T" ST #};
import enil: Texpr {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import newid: (Int->Texpr) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,T" ST #};

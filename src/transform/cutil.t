import mkchar: (Char->Texpr) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,T" ST #};
import mkint: (Int->Texpr) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,T" ST #};
import chkpat: (Texpr->Texpr) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import convstr: ((List Char)->Texpr) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import CXTuple: (Bool->(Int->Constr)) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,F" ST #};
import CTuple: (Int->Constr) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import idtoconstr: (Id->Constr) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import const: (String->(Ttype->(Tinfo->(Int->Texpr)))) {# ARITY _ = 4 #}{# STRICTNESS _ = "T,T" ST #};

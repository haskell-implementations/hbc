import isflat: ((List (#3 Constr *a *b))->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import anyused: ((List (#3 *a (List Id) *b))->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import cnoc: ((List (#3 Constr *a *b))->Int) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import isndg: (Id->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import Gcase: (Gmode->(Expr->((List (#3 Constr (List Id) Expr))->(Expr->(Genv->(Int->(Glabel->(Int->(Glabel->((List Gcode) # Glabel)))))))))) {# ARITY _ = 8 #}{# STRICTNESS _ = "T,F" ST #};

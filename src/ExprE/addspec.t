import addautospec: ((List (Id # (List (Id # Ttype))))->(Expr->(Int->(Expr # Int)))) {# ARITY _ = 3 #}{# STRICTNESS _ = "T,F" ST #};
import addspecrestr: (Expr->(Int->(Expr # Int))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import collectspec: (Subst->(Expr->(Int->((List (Id # (List (Id # Ttype)))) # Int)))) {# ARITY _ = 3 #}{# STRICTNESS _ = "1,F" ST #};
import addspec: ((List (Id # (List Pragma)))->(Expr->(Int->(Expr # Int)))) {# ARITY _ = 3 #}{# STRICTNESS _ = "T,F" ST #};

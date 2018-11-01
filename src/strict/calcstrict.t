import prstrict: ((List (Id # (#3 Int BT BT)))->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import calcstrict: (Expr->(List (Id # (#3 Int BT BT)))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};

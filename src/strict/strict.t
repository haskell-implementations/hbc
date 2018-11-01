import strictconvar: (Constr->((List *a)->(List *a))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import smkinfotab: ((List (Id # (#3 Int BT BT)))->(List (Int # Finfo))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import sidinfo: ((List (Int # Finfo))->(Id->Finfo)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import dostrictanal: (Expr->((List (Id # (#3 Int BT BT)))->((List (Id # (List Bool)))->Expr))) {# ARITY _ = 3 #}{# STRICTNESS _ = "0,F" ST #};
import strictanal: (Expr->((List (Id # (#3 Int BT BT)))->Expr)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};

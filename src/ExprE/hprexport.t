import prettypre: ((List (Id # (Ttype # (List Int))))->String) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import hprexport: ((List Id)->(Renv->(Expr->((List (Id # (#3 Int BT BT)))->(OK (List Char) (List Char)))))) {# ARITY _ = 4 #}{# STRICTNESS _ = "2,F" ST #};

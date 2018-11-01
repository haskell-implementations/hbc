import ecombTR: (Expr->(Subst->(Subst->Subst))) {# ARITY _ = 3 #}{# STRICTNESS _ = "1,F" ST #};
import ecombTRs: (Expr->((List Subst)->Subst)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import echk: (Expr->(Subst->Subst)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import Wdll: (Prefix->((List (List (Id # Expr)))->(Int->(#4 Subst Prefix Int (List (List (Id # Expr))))))) {# ARITY _ = 3 #}{# STRICTNESS _ = "T,T" ST #};

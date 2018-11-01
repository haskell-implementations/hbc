import appCgsT: ((Ttype->Ttype)->(Cgs->Cgs)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import instCgsTR: ((Ttype->Ttype)->(Subst->(Cgs->Cgs))) {# ARITY _ = 3 #}{# STRICTNESS _ = "2,F" ST #};
import prvb: ((List (Id # Ttype))->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import TRCgs: (Subst->(Cgs->Cgs)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import addTRs: ((List (Int # Ttype))->(Subst->Subst)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import TRvbind: (Subst->((List (*a # Ttype))->(List (*a # Ttype)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import prsubst: (Subst->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import instsubstTR: ((Ttype->Ttype)->(Subst->Subst)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};

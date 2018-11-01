import renin: (Renv->(IDecl->IDecl)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import makedefid: (Int->(Int->(String->(Id->(Ttype->((Id->Origname)->Id)))))) {# ARITY _ = 6 #}{# STRICTNESS _ = "T,F" ST #};
import makemetid: (Int->(Int->(String->(Classinfo->((Id->Origname)->Id))))) {# ARITY _ = 5 #}{# STRICTNESS _ = "T,T" ST #};
import reninst: (Bool->(Id->((Id->Origname)->(Renv->(Int->(Binding->(#3 Int Binding Renv))))))) {# ARITY _ = 6 #}{# STRICTNESS _ = "5,F" ST #};
import renclass: (Id->((Id->Origname)->(Renv->(Int->(Binding->(#3 Int Binding Renv)))))) {# ARITY _ = 5 #}{# STRICTNESS _ = "4,F" ST #};

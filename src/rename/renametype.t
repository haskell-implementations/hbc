import renbct: ((Id->Origname)->(Renv->(Int->(Ttype->((List Prod)->(#4 Int Ttype (List Atype) (Ttype # (List (IdOrConstr # Prod))))))))) {# ARITY _ = 5 #}{# STRICTNESS _ = "3,F" ST #};
import renbctype: ((Id->Origname)->(Renv->(Int->(Binding->(#3 Int Binding Renv))))) {# ARITY _ = 4 #}{# STRICTNESS _ = "3,F" ST #};
import syncirc: (Renv->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import rencontext: (Renv->(Assert->Assert)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import rensyntype: ((Id->Origname)->(Renv->(Int->(Binding->(#3 Int Binding Renv))))) {# ARITY _ = 4 #}{# STRICTNESS _ = "3,F" ST #};
import renbtenv: ((List Atype)->Renv) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import renbt: (Bool->((List Char)->((Id->Origname)->(Renv->(Int->(Ttype->((List Atype)->(#3 Int Ttype (List Atype))))))))) {# ARITY _ = 7 #}{# STRICTNESS _ = "0,F" ST #};
import renbtype: ((Id->Origname)->(Renv->(Int->(Binding->(#3 Int Binding Renv))))) {# ARITY _ = 4 #}{# STRICTNESS _ = "3,F" ST #};
import rentype: (Renv->(Ttype->Ttype)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};

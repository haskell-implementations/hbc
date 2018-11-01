import ivectorwrap: Id {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import i_ipreenv: Renv {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import i_unum1: Int {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import i_preenv: Renv {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import icomp: ((List Ttype)->(Renv->(Texpr->(Bool->(Bool->(Int->(OK (List Char) (#5 (List (Id # Cexpr)) Renv Renv Int (List Ttype))))))))) {# ARITY _ = 6 #}{# STRICTNESS _ = "T,F" ST #};

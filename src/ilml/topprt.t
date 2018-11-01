infixr  "$";
infixr  "@@";
import sigact: (Exception->(SigAct->(Dialog->Dialog))) {# ARITY _ = 4 #}{# STRICTNESS _ = "T,T" ST #};
import ($): ((*a->*b)->(*a->*b)) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,F" ST #};
import (@@): (String->(Dialog->Dialog)) {# ARITY _ = 3 #}{# STRICTNESS _ = "T,T" ST #};
import topprt: (Univ->(Dialog->Dialog)) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,F" ST #};

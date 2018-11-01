import itmake: (Int->((List (Id # *a))->(Idtab *a))) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,T" ST #};
import itlookup: ((Idtab *a)->(Id->*a)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import itlookupdef: ((Idtab *a)->(Id->(*a->*a))) {# ARITY _ = 3 #}{# STRICTNESS _ = "0&1,F" ST #};
import itadd1: ((Idtab *a)->(Id->(*a->(Idtab *a)))) {# ARITY _ = 3 #}{# STRICTNESS _ = "0&1,F" ST #};
import itnil: (Int->(Idtab *a)) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,T" ST #};

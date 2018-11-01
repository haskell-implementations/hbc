import Gu: (Int->(((List Glabel)->(Glabel->*a))->(Glabel->*a))) {# ARITY _ = 3 #}{# STRICTNESS _ = "2,F" ST #};
import Gi: (*a->(*b->(*a # *b))) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,T" ST #};
import Gseql: ((List (*a->(*b # *a)))->(*a->((List *b) # *a))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import Gseq: ((List (*a->((List *b) # *a)))->(*a->((List *b) # *a))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};

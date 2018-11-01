import pbselect: (Texpr->(Int->(Int->Texpr))) {# ARITY _ = 3 #}{# STRICTNESS _ = "T,T" ST #};
import selids: (List Id) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import pselect: (Texpr->(Texpr->(Texpr->Texpr))) {# ARITY _ = 3 #}{# STRICTNESS _ = "1,F" ST #};

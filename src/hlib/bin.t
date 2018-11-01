import PisNullBin: ((_List *a)->_Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import PnullBin: (_List *a) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import PreadBin: ((_List *a)->(*a # (_List *a))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import PshowBin: (*a->((_List *a)->(_List *a))) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,T" ST #};

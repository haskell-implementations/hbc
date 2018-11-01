import isNullBin: ((List *a)->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import readBin: ((List *a)->(*a # (List *a))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import nullBin: (List *a) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import showBin: (*a->((List *a)->(List *a))) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,T" ST #};

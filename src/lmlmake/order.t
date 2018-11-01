import closegraph: ((List (*a # (List *a)))->(List (*a # (List *a)))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import decorate: ((List (*a # *b))->((*c # (List (List *a)))->(*c # (List (List (*a # *b)))))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,1" ST #};
import scctsort: ((List (*a # (List *a)))->((List (List *a)) # (List (List *a)))) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,T" ST #};
import order: ((List (*a # (List *a)))->((List (List *a)) # (List (List (*a # (List *a)))))) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,T" ST #};

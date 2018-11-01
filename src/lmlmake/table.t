import emptyTable: (Table *a) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import tableLookup: (*a->(((*b # *c)->*a)->(*b->((Table (*b # *c))->*a)))) {# ARITY _ = 4 #}{# STRICTNESS _ = "3,F" ST #};
import tableUpdate: ((*a # *b)->((Table (*a # *b))->(Table (*a # *b)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import listTable: ((Table *a)->(List *a)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import mapTable: ((*a->*b)->((Table *a)->(Table *b))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import table: ((List (*a # *b))->(Table (*a # *b))) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};

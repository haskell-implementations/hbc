import impids: (Impid->(List Id)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import primpid: (Impid->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import prexpid: (Expid->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import expid: (Expid->Id) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};

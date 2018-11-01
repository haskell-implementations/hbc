import prWuses: (Int->((List Wuseinfo)->(List Char))) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,F" ST #};
import prWuse: (Wuseinfo->String) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import prstk: (Int->((List Addrmode)->(List Char))) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,F" ST #};
import pamode: (Addrmode->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import mprint: ((List Mcode)->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};

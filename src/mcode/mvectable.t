import addthemb: ((#3 (List (List Char)) (List (#3 Int Int Int)) (List Char))->((List Char)->((#3 (List (List Char)) (List (#3 Int Int Int)) (List Char)) # (#3 Int Int Int)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,0" ST #};
import addthem: ((#3 (List (List Char)) (List (#3 Int Int Int)) (List Char))->(((List Char) # (List Char))->((#3 (List (List Char)) (List (#3 Int Int Int)) (List Char)) # (#3 Int Int Int)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,0&1" ST #};
import v2l: ((#3 Int Int Int)->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import snum: ((List Char)->((List (List Char))->Int)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import addv: (*a->((List *a)->(List *a))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import adds: ((List *a)->((List *a)->(List *a))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import vectable: ((#3 (List (List Char)) (List (#3 Int Int Int)) *a)->(List Mcode)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};

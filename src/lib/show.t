import show_array: ((*a->(List Char))->((LArray *a)->(List Char))) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,F" ST #};
import show_pair: (((*a->(List Char)) # (*b->(List Char)))->((*a # *b)->(List Char))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import show_list: ((*a->(List Char))->((List *a)->(List Char))) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,F" ST #};
import show_option: ((*a->(List Char))->((Option *a)->(List Char))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import show_ok: ((*a->(List Char))->((*b->(List Char))->((OK *a *b)->(List Char)))) {# ARITY _ = 3 #}{# STRICTNESS _ = "2,F" ST #};
import show_string: ((List Char)->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import show_char: (Char->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import show_bool: (Bool->String) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import show_int: (Int->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};

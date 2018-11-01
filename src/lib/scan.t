import scan_pair: ((((List Char)->(*a # (List Char))) # ((List Char)->(*b # (List Char))))->((List Char)->((*a # *b) # (List Char)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import scan_list: (((List Char)->(*a # (List Char)))->((List Char)->((List *a) # (List Char)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import scan_string: ((List Char)->((List Char) # (List Char))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import scan_char: ((List Char)->(Char # (List Char))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import scan_bool: ((List Char)->(Bool # (List Char))) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import scan_int: ((List Char)->(Int # (List Char))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};

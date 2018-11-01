import readlist: (((List Char)->((List Char) # *a))->((List Char)->((List Char) # (List *a)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import readimport: ((List Char)->((List Char) # Import)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import readstring: ((List Char)->((List Char) # (List Char))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import readinterface: ((List Char)->((List Char) # Import)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import readimplist: ((List Char)->((List Char) # (List Impid))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import readbinding: ((List Char)->((List Char) # Binding)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import readexpr: ((List Char)->((List Char) # Texpr)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import readimpid: ((List Char)->((List Char) # Impid)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import Read: ((List Char)->(Texpr # (List ((List Char) # ((List Char) # Int))))) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,T" ST #};

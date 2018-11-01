import rused: (Int->((List Addrmode)->Bool)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import regof: (Addrmode->Int) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import gfreg: ((List Addrmode)->((List Addrmode)->Int)) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,F" ST #};
import gdreg: ((List Addrmode)->((List Addrmode)->Int)) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,F" ST #};
import gareg: ((List Addrmode)->((List Addrmode)->Int)) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,F" ST #};

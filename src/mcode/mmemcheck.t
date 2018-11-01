import saverestore: (((List Int) # (List Int))->((List Mcode) # (List Mcode))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import allusedregs: ((List Addrmode)->((List Addrmode)->((List Int)->((List Int) # (List Int))))) {# ARITY _ = 3 #}{# STRICTNESS _ = "T,T" ST #};
import mmemcheck: ((#3 (List (List Char)) (List (#3 Int Int Int)) String)->(Int->((List Gcode)->((List Addrmode)->((List Addrmode)->(#5 (List Mcode) (List (List Char)) (List Wuseinfo) (List Wuseinfo) Int)))))) {# ARITY _ = 5 #}{# STRICTNESS _ = "T,F" ST #};

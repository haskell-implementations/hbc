import M: ((#3 (List (List Char)) (List (#3 Int Int Int)) String)->(Int->(Int->((List Gcode)->((List Addrmode)->((List Addrmode)->(#5 (List Mcode) (List (List Char)) (List Wuseinfo) (List Wuseinfo) Int))))))) {# ARITY _ = 6 #}{# STRICTNESS _ = "2&3,F" ST #};
import mmain: ((List (#3 (Id # Int) (List Gcode) (Option (List Gcode))))->(List Mcode)) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};

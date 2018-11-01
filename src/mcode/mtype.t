import msplit: ((List Bool)->(Int->(Int->(List Gcode)))) {# ARITY _ = 3 #}{# STRICTNESS _ = "2,2" ST #};
import mconstr: (HPinfo->(Int->(Int->(List Gcode)))) {# ARITY _ = 3 #}{# STRICTNESS _ = "2,F" ST #};

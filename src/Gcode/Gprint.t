import Gprints: ((List (#3 (Id # Int) (List Gcode) (Option (List Gcode))))->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import Gprint: ((List Gcode)->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};

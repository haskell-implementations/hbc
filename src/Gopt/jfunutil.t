import eqGs: ((List Gcode)->((List Gcode)->Bool)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import refcnt: (Int->((List Int)->Int)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import delref: (Int->((List Int)->(List Int))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import reflist: ((List Gcode)->(List Int)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};

import addlabel: ((#3 Int *a (List Gcode))->(List Gcode)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import extractb: (Int->((List (#3 Int *a *b))->(#3 Int *a *b))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import bopt: ((#3 *a Int (List Gcode))->(#3 *a Int (List Gcode))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import bblock: ((List Gcode)->((#3 Int Int (List Gcode)) # (List Gcode))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import bhead: ((List Gcode)->Int) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import bend: (Int->((List Gcode)->(Int # (List Gcode)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import empty: ((#3 *a *b (List *c))->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import nlab: (Gcode->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};

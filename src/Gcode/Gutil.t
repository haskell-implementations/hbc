import cleanG: ((List Gcode)->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import Eisconst: (Expr->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import Gdoeval: ((List Id)->(Genv->(Int->(*a->((List Gcode) # *a))))) {# ARITY _ = 4 #}{# STRICTNESS _ = "0,F" ST #};
import lstrop: (Int->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import bigop: (Int->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import typenamefromtinfo: (Tinfo->String) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import gnoargs: (Id->Int) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import prebasicop: (Int->Gbasicop) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import prebasicconstr: (Int->Gbasicconstr) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import prebasicarg: (Int->Gbasicconstr) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import updstack: (Id->(Genv->(Int->(*a->((List Gcode) # *a))))) {# ARITY _ = 3 #}{# STRICTNESS _ = "T,F" ST #};
import pushvar: (Id->(Genv->(Int->(Int->(*a->((List Gcode) # *a)))))) {# ARITY _ = 5 #}{# STRICTNESS _ = "T,F" ST #};
import Gcpushrev: ((List Expr)->(Genv->(Int->(Glabel->((List Gcode) # Glabel))))) {# ARITY _ = 4 #}{# STRICTNESS _ = "T,F" ST #};
import Gslide: (Int->(*a->((List Gcode) # *a))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import Gpop: (Int->(*a->((List Gcode) # *a))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import Gjmp: (Gmode->(Glabel->(*a->((List Gcode) # *a)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};

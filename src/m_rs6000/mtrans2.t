import smallh: (Int->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import small: (Int->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import mtrans2: ((List Mcode)->(List Mcode)) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};

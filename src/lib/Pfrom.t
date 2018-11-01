import Praise: (_Int->(_Int->_Int)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import Psel: ((_List *a)->(_Int->*a)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import Pfrombyto: (_Int->(_Int->(_Int->(_List _Int)))) {# ARITY _ = 3 #}{# STRICTNESS _ = "0&1&2,F" ST #};
import Pfromto: (_Int->(_Int->(_List _Int))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import Pfromby: (_Int->(_Int->(_List _Int))) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,F" ST #};
import Pfrom: (_Int->(_List _Int)) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};

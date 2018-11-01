import Clear: ((List) (Char)) {# ARITY Clear = 0 #}{# STRICTNESS Clear = "F,T" ST #};
import MoveTo: ((Int)->((Int)->((List) (Char)))) {# ARITY MoveTo = 2 #}{# STRICTNESS MoveTo = "0&1,0&1" ST #};

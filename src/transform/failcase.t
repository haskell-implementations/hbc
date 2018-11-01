import tfail: (String->Texpr) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,T" ST #};
import fmkcase: (Texpr->((List (Texpr # Texpr))->Texpr)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};

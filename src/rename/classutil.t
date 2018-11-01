import getclassinfo: (Kind->(Renv->(CDecl->((List (#3 Id Id Ttype))->Classinfo)))) {# ARITY _ = 4 #}{# STRICTNESS _ = "2,2" ST #};
import gettypeinsts: (Renv->(Id->(List (Id # IDecl)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import sortcon: ((List Assert)->(List Assert)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};

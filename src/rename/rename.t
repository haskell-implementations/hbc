import expandrecord: (Bool->(Texpr->((List Id)->((List Texpr)->Texpr)))) {# ARITY _ = 4 #}{# STRICTNESS _ = "2,F" ST #};
import renpat: (Id->((Id->Origname)->(Renv->(Int->(Texpr->(Int # Texpr)))))) {# ARITY _ = 5 #}{# STRICTNESS _ = "4,F" ST #};
import renexpr: (Id->((Id->Origname)->(Renv->(Int->(Texpr->(Int # Texpr)))))) {# ARITY _ = 5 #}{# STRICTNESS _ = "4,F" ST #};
import rename: (Int->(Texpr->(#6 Int (List Id) Renv (List (Id # (List Pragma))) String Texpr))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};

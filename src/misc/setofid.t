import Idi: ((List Id)->((List Id)->(List Id))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import Is: ((List Id)->((List Id)->(List Id))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import Iu: ((List Id)->((List Id)->(List Id))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import Imem: (Id->((List Id)->Bool)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import Imk: ((List Id)->(List Id)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import Ieq: (Id->(Id->Bool)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import Igt: (Id->(Id->Bool)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import Ilt: (Id->(Id->Bool)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};

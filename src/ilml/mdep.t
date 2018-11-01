import Vcno: Cexpr {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import splt: (Int->(Int->(Cexpr->Cexpr))) {# ARITY _ = 3 #}{# STRICTNESS _ = "1,0&1" ST #};
import Vtst: Cexpr {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import Vtstf: Cexpr {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import imkint: (Int->Cexpr) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,T" ST #};
import con: *a {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};

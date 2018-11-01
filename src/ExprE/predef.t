import bigordops: (List Id) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import bigeqops: (List Id) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import pre_neg: Id {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import pre_sub: Id {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import pre_add: Id {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import predef: (Expr->Expr) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};

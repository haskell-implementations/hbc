import ppclose: Char {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import ppopen: Char {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import type pptype = PPnode (List Char) (List pptype);

import substbt: ((List BT)->(BT->BT)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import btff: BT {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import bttt: BT {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import btsimpl: (BT->BT) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import show_BT: (BT->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import type BT = btvar Int + btands (List BT) + btors (List BT) + btapp Int (List BT) + btfunarg Int Int;

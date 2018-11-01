import showBin: (*a->(Bin->Bin)) {# ARITY showBin = 2 #}{# STRICTNESS showBin = "F,F" ST #};
import readBin: (Bin->(*a # Bin)) {# ARITY readBin = 1 #}{# STRICTNESS readBin = "0,F" ST #};
import nullBin: (Bin) {# ARITY nullBin = 0 #}{# STRICTNESS nullBin = "F,T" ST #};
import isNullBin: (Bin->(Bool)) {# ARITY isNullBin = 1 #}{# STRICTNESS isNullBin = "0,0" ST #};

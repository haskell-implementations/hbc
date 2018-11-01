import splitNames: (nameSupply->(nameSupply # nameSupply)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import getName: (nameSupply->Int) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import initialNameSupply: nameSupply {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};

infix   "oo";
import issubset: ((List *a)->((List *a)->Bool)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import part: ((*a->Bool)->((List *a)->((List *a) # (List *a)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import lsplit: ((List *a)->((List *b)->((List *b) # (List *b)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import ltail: ((List *a)->((List *b)->(List *b))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import lhead: ((List *a)->((List *b)->(List *b))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import replace: ((*a # *b)->((List (*a # *b))->(List (*a # *b)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import remove: (*a->((List *a)->(List *a))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import unionmap: ((*a->(List *b))->((List *a)->(List *b))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import gmap: ((*a->((List *b)->(List *b)))->((*c->*a)->((List *c)->(List *b)))) {# ARITY _ = 3 #}{# STRICTNESS _ = "2,F" ST #};
import loop: ((*a->*a)->*a) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import C: ((*a->(*b->*c))->(*b->(*a->*c))) {# ARITY _ = 3 #}{# STRICTNESS _ = "T,F" ST #};
import id: (*a->*a) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import const: (*a->(*b->*a)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,0" ST #};
import number: (Int->((List *a)->(List (Int # *a)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import dropto: ((*a->Bool)->((List *a)->(List *a))) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,F" ST #};
import anth: (Int->((*a->*a)->((List *a)->(List *a)))) {# ARITY _ = 3 #}{# STRICTNESS _ = "2,F" ST #};
import (oo): ((*a->*b)->((*c->(*d->*a))->(*c->(*d->*b)))) {# ARITY _ = 4 #}{# STRICTNESS _ = "T,F" ST #};
import swap: ((*a # *b)->(*b # *a)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import pairwith: ((*a->*b)->(*a->(*a # *b))) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,T" ST #};
import apair: (((*a->*b) # (*c->*d))->((*a # *c)->(*b # *d))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,0&1" ST #};
import aboth: ((*a->*b)->((*a # *a)->(*b # *b))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,1" ST #};
import asnd: ((*a->*b)->((*c # *a)->(*c # *b))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,1" ST #};
import afst: ((*a->*b)->((*a # *c)->(*b # *c))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,1" ST #};

import filemodtime: ((List Char)->When) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import isOld: (When->(When->Bool)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import show_When: (When->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import type When = Never + At Int;

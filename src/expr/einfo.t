import pr1entry: ((Arginfo # Argpos)->Char) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import f_unk: Finfo {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import entries_of_finfo: (Finfo->(List (List (Arginfo # Argpos)))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import hprfinfo: (Bool->(String->(Finfo->(List Char)))) {# ARITY _ = 3 #}{# STRICTNESS _ = "2,F" ST #};
import arity_of_finfo: (Finfo->Int) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import framesize_of_finfo: (Finfo->Int) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import prfinfo: (Finfo->(String->(List Char))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import preinfo: (Teinfo->((List Char)->(List Char))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};

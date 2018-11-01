import rgetkind: (Id->(Renv->Kind)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import raddkt: ((List (Id # Kind))->(Renv->Renv)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,1" ST #};
import raddpragma: (Renv->((List (Id # (List Pragma)))->Renv)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,0" ST #};
import rgetpragma: (Renv->(List (Id # (List Pragma)))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import rgetcenv: (Renv->(List (Ttype # (List (IdOrConstr # Prod))))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import rcone: ((Ttype # (List (IdOrConstr # Prod)))->Renv) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,T" ST #};
import rgetct: (Renv->(#3 (List (Id # (List (Id # IDecl)))) (List (Id # (List (Id # IDecl)))) (List (Id # (List (Id # (List Int))))))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import rsetct: (Renv->((#3 (List (Id # (List (Id # IDecl)))) (List (Id # (List (Id # IDecl)))) (List (Id # (List (Id # (List Int))))))->Renv)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,0" ST #};
import rfindselid: (String->(Renv->Id)) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,F" ST #};
import rdbldefs: (RKind->(Renv->(Renv->(List String)))) {# ARITY _ = 3 #}{# STRICTNESS _ = "T,F" ST #};
import rmkonametbl: (Renv->Renv) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import rfindid: (RKind->(Id->(Renv->Id))) {# ARITY _ = 3 #}{# STRICTNESS _ = "0,F" ST #};
import rjoin1type: (Renv->(Id->Renv)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,0" ST #};
import rmaptemp: ((Id->Id)->(Renv->Renv)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,1" ST #};
import envinfo: (Renv->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import rstrs: (RKind->(Renv->(List String))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import show_Renv: (Renv->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import type RKind = Kvalue + Ktype + Kmodule + Kmeth + Kall;
import rperm: (Renv->Renv) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import rids: (RKind->(Renv->(List Id))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import rfind: (RKind->(String->(Renv->Id))) {# ARITY _ = 3 #}{# STRICTNESS _ = "0,F" ST #};
import rlist: (RKind->((List Id)->Renv)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import rjoin1: (Renv->(RKind->(Id->Renv))) {# ARITY _ = 3 #}{# STRICTNESS _ = "0&1,F" ST #};
import rjoin: (Renv->(Renv->Renv)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import rmany: (RKind->((List Id)->Renv)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import rone: (RKind->(Id->Renv)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import rnil: Renv {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
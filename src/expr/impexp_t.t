import type Impid = mkimpid Id Ttype Finfo (Option (List (Ttype # Finfo))) + mkimptype Kind Ttype (Int # Bool) + mkimpeqtype Kind Ttype (List Atype) (Option (List Id)) Bool Bool + mkimpsyn Kind Ttype Ttype + mkimpclass Kind CDecl Binding (List (Id # Finfo)) + mkimpinstance IDecl Bool (List (Id # Finfo)) + mkimpimport Id (List Expid) (List (Id # Id)) + mkimpids (List Id) Ttype Finfo + mkimpctype Ttype (List Prod) + mkimpview Kind Ttype Ttype (List Atype);
import type Expid = mkexpid Id + mkexpidall Id + mkexpidsome Id (List Id) + mkexpidmodall Id;
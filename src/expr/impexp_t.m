module
export Expid, Impid;
rec type Expid =
          mkexpid Id
	+ mkexpidall Id
	+ mkexpidsome Id (List Id)
	+ mkexpidmodall Id
and type Impid =
	  mkimpid Id Ttype Finfo (Option (List (Ttype#Finfo)))
	+ mkimptype Kind Ttype (Int # Bool)
	+ mkimpeqtype Kind Ttype (List Atype) (Option (List Id)) Bool Bool     -- if last flag is false it should only be completely visible with qnames
	+ mkimpsyn Kind Ttype Ttype
	+ mkimpclass Kind CDecl Binding (List (Id#Finfo))
	+ mkimpinstance IDecl Bool (List (Id#Finfo))
	+ mkimpimport Id (List Expid) (List (Id # Id))
	+ mkimpids (List Id) Ttype Finfo
	+ mkimpctype Ttype (List Prod)
        + mkimpview Kind Ttype Ttype (List Atype)
end

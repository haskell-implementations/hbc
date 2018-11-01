module
export Ttype, CDecl, IDecl, Assert, TyVar, Context, Kind;
rec type TyVar == Int
and type Context == List Assert
and type Ttype =
	  mktcons Id (List Ttype)
	+ mktvar TyVar
	+ mkterror (List Char)
	+ mktcontype Context Ttype	-- Only on top level
        + mktap TyVar (List Ttype)
and type CDecl =
    	  mkcdecl Context Assert
and type IDecl =
          mkidecl Context Id [Ttype] String	-- Last is original module name
and type Assert =
          mkassert Id (List TyVar)
        + mkaerror (List Char)
and type Kind =
          mkkarrow Kind Kind
        + mkkground
        + mkkvar Int			-- used during kind deduction
end

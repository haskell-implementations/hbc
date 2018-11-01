module
export Constr, Tinfo, ConstructorType;
rec type Constr =
	-- print name, constructor type, information about the type,
	-- constructor number, (existential vars, existential context),
        -- list parameter types (all generic) and strictness flag
	--
	-- The constructor type is derivable from the arg type and the type
	-- the constructor belongs to.
	Cconstr String Ttype Tinfo Int (Bool # (List Int) # (List Assert)) (List (Ttype#Bool))

and type Tinfo =
	-- information about a type:
	-- type, number of constructors, non-free flag, flat flag, all constructors, has existential constr, isomorphic type
	mktinfo Ttype Int Bool Bool (List Atype) Bool Bool (Option Id)

and type ConstructorType = Gint + Gchar + Gstring String + Gtype + Gdfloat String + Ginteger String + Gsfloat String
end

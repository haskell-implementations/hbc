module
export Id, Idinfo, Varinfo, Classinfo, Origname, Fixity, Visibility, Otype, Modinfo;
rec type Id =
	mkids String +					-- before rename, just name
	mkidi String (Option (Modinfo # String)) String [String] +	-- during interface processing, name & original name, name of module that caused import, prefixes that the id should be known under
	mkid Int String Idinfo Origname		-- after rename, all kinds of info
and type Idinfo =
	idi_udef +
	idi_var Varinfo Otype (Option Id) +		-- last is type id for a selector
	/* constructor type, (exist var, exist context), parameter type list, constr no, other con*/
	idi_constr Ttype (Bool#(List Int)#(List Assert)) (List(Ttype#Bool#(Option Id))) Int (List Atype) Bool (Option Id) +
	idi_type Kind Ttype Int Tinfo (List (Id#IDecl)) (Option (List Id))+		-- Type (with context), # of tvars, constructors, class belongings, derived insts
        idi_view Ttype Id Idinfo +				-- viewed type, view transformation, idi_type for the view
	idi_conctype Ttype (List Prod) +			-- The productions of a conctype
	idi_syn Kind Ttype Int Ttype +				-- Source (pattern), # of tvars, destination (body)
	idi_class Classinfo +
	idi_method (List Int) Int Classinfo +			-- Method number, arity, class info
	idi_inst IDecl (List Id) Bool +				-- Instance vector: instance declaration, method ids, derived inst flag
	idi_module (List Expid)					-- Module (with visible ids)
and type Varinfo =
	var_unknown +
	var_local Int +		-- binding depth
	var_global Finfo +	-- visible, arity (<0 indicates unknown), frame size
	var_pre Finfo +		-- predefined (known by the compiler), with function info
	var_dict Classinfo	-- class info for lambda bound dictionary
and type Classinfo =
        clsi Kind CDecl (List (Id#Id#Ttype)) (List (Id#(List Int))) (List (Id#IDecl)) Int		-- Type (with context), methods (default name, method name, type), superclasses, instances (=method vector,instance decl type), # of immediate superclasses
and type Fixity = Infix Int + InfixL Int + InfixR Int + Nofixity + FPrefix Int + FPostfix Int + Nonfix
and type Origname = Noorigname + Orignames Visibility Fixity (Modinfo # String)
and type Visibility = Vimported + Vexported + Vprivate
and type Otype = Onotype + Ohastype Ttype (List Int) (Option (List (Id#Ttype#Finfo)))
and type Modinfo = MI String
end

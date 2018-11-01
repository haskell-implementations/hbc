module
export Flic, Annot, Name;
rec
type Name == String
and
type Flic = Fname Name			-- identifier
	  + Fnumber Int			-- number
	  + Fchar Char			-- char
	  + Fstring String		-- string
	  + Fap Flic Flic		-- application
	  + Flam Name Flic		-- lambda abstraction
	  + Flet Bool (List Name) (List Flic) Flic	
					-- let or letrec
					-- Recursive if first arg is true
	  + Fannot Annot Flic
	  + Ffail			-- used by parser only
and
type Annot = Annot0 Name
	  + Annot1 Name Flic
end

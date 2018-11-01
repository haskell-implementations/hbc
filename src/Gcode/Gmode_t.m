module
-- 
-- Modes of G code generation
--
export Gmode;
type Gmode = 
	R + 		-- Return value
	E Int + 	-- Evaluate to top of stack, and slide to stackdepth
	B Int Gbasicconstr + 	-- Evaluate basic value, and pop to stackdepth
	J Int (List(Int#(Int#Glabel))) + -- constr no, stack depth, label
	C Int +		-- Construct and slide to stackdepth.
	SE Int +        -- Stingy E-scheme
	SB Int         -- Stingy B-scheme
end

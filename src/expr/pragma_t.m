module
export Pragma;
rec type Pragma = 
	  Pspecialize Id (List (Ttype#(Option Id)))
	+ Pspecinst Ttype
end

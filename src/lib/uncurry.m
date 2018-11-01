/*
**	uncurry:	"uncurries" a function
*/
module
export	uncurry;
rec

   uncurry f (a,b) = f a b
end

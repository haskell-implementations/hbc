module
#include "../misc/setofid.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
export mstrid, mexported;
rec
/* Convert an identifier to a string
 *
 * If the identifier is global (exported or imported identifiers),
 * the the identifier is
 * represented by the name originally given in the source program, eg Cfoo ;
 * otherwise an internal name is prepended, based on its unique number,
 * eg LF123_foo .
 */
    mexported i = id_is_visible i
and
    mstrid i =
	-- We have to fix strange characters in the names
	if mexported i then 
	    "C" @ asmid i
	else
	    "LF" @ itos (id_no i) @ asmid i
end

/*
**	termcap		interface to termcap database
**	Clear:		string to clear the screen
**
**	MoveTo x y:	string to move to coordinate x,y
*/
module
#include "termcap_M.t"
export	Clear, MoveTo, Lines, Columns;
    Clear = _Clear
and MoveTo x y = _MoveTo x y
and Lines = _Lines
and Columns = _Columns
end

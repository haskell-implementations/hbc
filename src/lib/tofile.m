/*
**	stofile:		redirect output to a file
**
*/
module
#define TOFILE(name)   (chr(-256).force (name)@"\n")
#define TOFILEA(name)   (chr(-257).force (name)@"\n")
export stofile, stofilea;
    stofile n = TOFILE(n)
and stofilea n = TOFILEA(n)
end

module
export	pptype, ppopen, ppclose;
#define list		List
#define char		Char
#define	string		(list char)
--
   (rec type pptype  =  PPnode string (list pptype))
and
   ppopen	=  chr 1
and
   ppclose	=  chr 2
end

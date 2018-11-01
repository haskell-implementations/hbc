module	-- getmodtime -
#include "fs.h"
export When,show_When,isOld,filemodtime;

rec type When = Never + At Int

and show_When Never = "never"
 || show_When (At t) = itos t

and isOld Never _ = true
 || isOld _ Never = false -- missing source file?
 || isOld (At t1) (At t2) = t1<t2

and filemodtime filename =
  case statfile filename
  in Yes stat: At (select 10 stat)	-- extract file modification time
  || No _ : Never	-- file probably doesn't exist...
  end

end

module
#include "parse.t"

#include "pptype.t"
#include "ppadd.t"

export ppparse;
rec
   ppparse =
      (nxt ppunit (\v___1.
      (nxtok EOF (\v___2.
      (ok (v___1))))))
and
   ppunit =
      (nxt ppstring (\v___1.
      (nxtok pplist (\v___2.
      (ok (PPnode v___1 v___2))))))
and
   pplist =
      (alt (nxt ppitem (\v___1.
           (nxtok pplist (\v___2.
           (ok (v___1 . v___2))))))
      (ok ([ ])))
and
   ppitem =
      (nxt PPOPEN (\v___1.
      (nxtok ppunit (\v___2.
      (nxtok PPCLOSE (\v___3.
      (nxtok ppstring (\v___4.
      (ok (ppadd v___2 v___4))))))))))
and
   ppstring =
      (alt (nxt PPCHAR (\v___1.
           (nxtok ppstring (\v___2.
           (ok (v___1 . v___2))))))
      (ok ([ ])))
and

   EOF		=  lit eof
and
   PPOPEN	=  lit ppopen
and
   PPCLOSE	=  lit ppclose
and
   PPCHAR	=  litp (\c. c ~= ppopen & c ~= ppclose & c ~= eof)
and
   eof		=  chr 0

end

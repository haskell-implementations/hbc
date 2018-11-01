#define dtmp1 (reg 8)
#define dtmp1ind (regind 8 0)
#define dtmp2 (reg 9)
#define dtmp2ind (regind 9 0)

#define ArgCReg  2
#define TagReg   2
#define BigEqReg 2
#define IndReg   4
#define GCSTART  5
#define GCEND    6
#define GCCUR    7
#define Ehpr 10
#define hpr  11
#define Spr  12
#define Vpr  13
#define Ret  14
#define Pc   15

/* From tmp.h                 From  runtime/machdep.h    From machine.m 
    0 = r0                       &  CRET
    1 = r1
    2 = ArgCReg,TagReg,BigEqReg  &  ATMP                    & Aregs,Dregs
    3 =                             DTMP,CDTMP              & Aregs,Dregs
    4 = IndReg                   &  INDREG                  & Aregs,Dregs
    5 =                             GCSTART                 & Aregs,Dregs
    6 =                             GCEND                   & Aregs,Dregs
    7 =                             GCCUR                   & Aregs,Dregs
    8 = dtmp1
    9 = dtmp2                    &  rd
   10 = Ehpr
   11 = hpr
sp 12 = Spr
ip 13 = Vpr
lr 14 = Ret                      &  FDISP
pc 15 = Pc
*/

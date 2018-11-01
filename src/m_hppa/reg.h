/* All regs are offset by 8 to avoid having the permanent 0 in r0 */
/* System regs */
#define Rzero 24	/* %r0 */
#define Rret 26		/* %r2 */
#define Rfp 28		/* %r4 */
#define Rsp 22		/* %r30 */
#define Rarg0 18	/* %r26 */
#define Rarg1 17	/* %r25 */
#define Rres 20		/* %r28 */
#define Rres1 21	/* %r29 */

/* Unfortunately the HP stack grows up, so we use our own instead */
/* M-regs */
#define dtmp1 (reg 13)
#define dtmp2 (reg 14)
#define dtmp3 (reg 15)
#define dtmp4 (reg 16)
#define Ftmp1 (reg (FP+8))
#define Ftmp2 (reg (FP+9))
#define Ftmp3 (reg (FP+10))
#define Ftmp4 (reg (FP+11))
#define FP 32
#define FPL 64
#define FPR 96
#define Fmul1 (reg (FPL+14))
#define Fmul2 (reg (FPR+14))
#define Fmul2d (reg (FP+14))
#define Spr 29
#define Vpr 30
#define hpr 31
#define Ret Rret
#define Ehpr 12
#define Canonr 7

#define WSIZE 4

/* m-co machine
** 0-6	%r8-%r14 M-code regs
** 7    %r15    Canonr
** 8-10 %r16-%r18 M-code, used during gc
** 11   %r19    indreg
** 12	%r20	Ehpr
** 13	%r21	dtmp1
** 14	%r22	dtmp2
** 15	%r23	dtmp3 arg
** 16	%r24	dtmp4 arg
** 17	%r25	Rarg1
** 18	%r26	Rarg0
** 19	%r27	? glob data
** 20	%r28	Rres
** 21	%r29	? result/static chain
** 22	%r30	Rsp
** 23	%r31	? millicode?
** 24	%r0	Rzero
** 25	%r1	temp
** 26	%r2	Rret
** 27	%r3	? frame
** 28	%r4	Rfp
** 29	%r5	Spr
** 30	%r6	Vpr
** 31	%r7	hpr
** 32-35%f0-%f3	status
** 36-39%f4-%f7 arg/ret
** 40-45%f8-%f13 ftmp*
** 46,47%f14,%f15 multiply temps
** 48-63%f16-%f31 M-code fpregs
*/

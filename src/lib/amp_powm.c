/*

Copyright (C) 1991 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

The GNU MP Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with the GNU MP Library; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

* Collected from GMP 1.2 and modified for LML by Lennart Augustsson.

*/

/* res = amp_powm(base,exp,mod) -- Set RES to (base**exp) mod MOD. */

#include "amp.h"

AMP*
#if __STDC__
amp_powm (AMP *base, AMP *exp, AMP *mod)
#else
amp_powm (base, exp, mod)
     AMP *base;
     AMP *exp;
     AMP *mod;
#endif
{
  AMP *res;
  mp_ptr rp, ep, mp, bp;
  mp_size esize, msize, bsize, rsize;
  mp_size size;
  int mod_shift_cnt;
  int negative_result;

  esize = abs (exp->size);
  msize = abs (mod->size);
  size = 2 * msize;

  ep = exp->d;

  /* Normalize MOD (i.e. make its most significant bit set) as required by
     _mpn_div.  This will make the intermediate values in the calculation
     slightly larger, but the correct result is obtained after a final
     reduction using the original MOD value.  */

  mp = (mp_ptr) alloca (msize * BYTES_PER_MP_LIMB);
  count_leading_zeros (mod_shift_cnt, mod->d[msize - 1]);
  if (mod_shift_cnt != 0)
    (void) _mpn_lshift (mp, mod->d, msize, mod_shift_cnt);
  else
    MPN_COPY (mp, mod->d, msize);

  bsize = abs (base->size);
  if (bsize > msize)
    {
      /* The base is larger than the module.  Reduce it.  */

      bp = (mp_ptr) alloca (bsize * BYTES_PER_MP_LIMB);
      MPN_COPY (bp, base->d, bsize);
      /* We don't care about the quotient, store it above the remainder,
	 at BP + MSIZE.  */
      _mpn_div (bp + msize, bp, bsize, mp, msize);
      bsize = msize;
      while (bsize > 0 && bp[bsize - 1] == 0)
	bsize--;
    }
  else
    {
      bp = base->d;
      bsize = abs (base->size);
    }

  res = amp_alloc(size * BYTES_PER_MP_LIMB);
  rp = res->d;

  if (esize == 0)
    {
      rp[0] = 1;
      rsize = 1;
      return res;
    }

  MPN_COPY (rp, bp, bsize);
  rsize = bsize;

  {
    mp_size i;
    mp_size xsize;
    mp_ptr dummyp = (mp_ptr) alloca ((msize + 1) * BYTES_PER_MP_LIMB);
    mp_ptr xp = (mp_ptr) alloca (2 * (msize + 1) * BYTES_PER_MP_LIMB);
    int c;
    mp_limb e;
    mp_limb carry_digit;

    negative_result = (ep[0] & 1) && base->size < 0;

    i = esize - 1;
    e = ep[i];
    count_leading_zeros (c, e);
    e <<= (c + 1);		/* shift the exp bits to the left, loose msb */
    c = BITS_PER_MP_LIMB - 1 - c;

    /* Main loop.

       Make the result be pointed to by either XP or RP.  This
       helps us avoid block copying, which would otherwise be necessary
       with the overlap resrictions of _mpn_div.  With 50% probabillity
       the result after this loop will be in the area originally pointed
       by RP (==RES->D), and with 50% probabillity in the area originally
       pointed to by XP.  */

    for (;;)
      {
	while (c != 0)
	  {
	    mp_ptr tp;
	    mp_size tsize;

	    xsize = _mpn_mul (xp, rp, rsize, rp, rsize);
	    _mpn_div (dummyp, xp, xsize, mp, msize);

	    /* Remove any leading zero words from the result.  */
	    if (xsize > msize)
	      xsize = msize;
	    while (xsize > 0 && xp[xsize - 1] == 0)
	      xsize--;

	    tp = rp; rp = xp; xp = tp;
	    tsize = rsize; rsize = xsize; xsize = tsize;

	    if ((mp_limb_signed) e < 0)
	      {
		if (rsize > bsize)
		  xsize = _mpn_mul (xp, rp, rsize, bp, bsize);
		else
		  xsize = _mpn_mul (xp, bp, bsize, rp, rsize);
		_mpn_div (dummyp, xp, xsize, mp, msize);

		/* Remove any leading zero words from the result.  */
		if (xsize > msize)
		  xsize = msize;
		while (xsize > 0 && xp[xsize - 1] == 0)
		  xsize--;

		tp = rp; rp = xp; xp = tp;
		tsize = rsize; rsize = xsize; xsize = tsize;
	      }
	    e <<= 1;
	    c--;
	  }

	i--;
	if (i < 0)
	  break;
	e = ep[i];
	c = BITS_PER_MP_LIMB;
      }

    /* We shifted MOD, the modulo reduction argument, left MOD_SHIFT_CNT
       steps.  Adjust the result by reducing it with the original MOD.

       Also make sure the result is put in RES->D (where it already */

    carry_digit = _mpn_lshift (res->d, rp, rsize, mod_shift_cnt);
    rp = res->d;
    if (carry_digit != 0)
      {
	rp[rsize] = carry_digit;
	rsize++;
      }
    _mpn_div (dummyp, rp, rsize, mp, msize);
    /* Remove any leading zero words from the result.  */
    if (rsize > msize)
      rsize = msize;
    while (rsize > 0 && rp[rsize - 1] == 0)
      rsize--;
    rsize = _mpn_rshift (rp, rp, rsize, mod_shift_cnt);
  }

  res->size = negative_result >= 0 ?  rsize : -rsize;

  return res;
}


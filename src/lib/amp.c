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

#include "amp.h"

AMP*
#if __STDC__
amp_add (AMP *u, AMP *v)
#else
amp_add (u, v)
     AMP *u;
     AMP *v;
#endif
{
  AMP *sum;
  mp_srcptr up, vp;
  mp_ptr sump;
  mp_size usize, vsize, sumsize;
  mp_size abs_usize;
  mp_size abs_vsize;

  usize = u->size;
  vsize = v->size;
  abs_usize = abs (usize);
  abs_vsize = abs (vsize);

  if (abs_usize < abs_vsize)
    {
      /* Swap U and V. */
      {AMP *t = u; u = v; v = t;}
      {mp_size t = usize; usize = vsize; vsize = t;}
      {mp_size t = abs_usize; abs_usize = abs_vsize; abs_vsize = t;}
    }

  /* True: abs(USIZE) >= abs(VSIZE) */

  /* If not space for sum (and possible carry), increase space.  */
  sumsize = abs_usize + 1;
  sum = amp_alloc (sumsize);

  /* These must be after realloc (u or v may be the same as sum).  */
  up = u->d;
  vp = v->d;
  sump = sum->d;

  if (usize >= 0)
    {
      if (vsize >= 0)
	{
	  sumsize = _mpn_add (sump, up, abs_usize, vp, abs_vsize);
	  if (sumsize != 0)
	    sump[abs_usize] = 1;
	  sumsize = sumsize + abs_usize;
	}
      else
	{
	  /* The signs are different.  Need exact comparision to determine
	     which operand to subtract from which.  */
	  if (abs_usize == abs_vsize
	      && _mpn_cmp (up, abs_usize, vp, abs_usize) < 0)
	    sumsize = -(abs_usize
			+ _mpn_sub (sump, vp, abs_usize, up, abs_usize));
	  else
	    sumsize = (abs_usize
		       + _mpn_sub (sump, up, abs_usize, vp, abs_vsize));
	}
    }
  else
    {
      if (vsize >= 0)
	{
	  /* The signs are different.  Need exact comparision to determine
	     which operand to subtract from which.  */
	  if (abs_usize == abs_vsize
	      && _mpn_cmp (up, abs_usize, vp, abs_usize) < 0)
	    sumsize = (abs_usize
		       + _mpn_sub (sump, vp, abs_usize, up, abs_usize));
	  else
	    sumsize = -(abs_usize
			+ _mpn_sub (sump, up, abs_usize, vp, abs_vsize));
	}
      else
	{
	  sumsize = _mpn_add (sump, up, abs_usize, vp, abs_vsize);
	  if (sumsize != 0)
	    sump[abs_usize] = 1;
	  sumsize = -(sumsize + abs_usize);
	}
    }

  sum->size = sumsize;
  return sum;
}

int
#if __STDC__
amp_cmp (AMP *u, AMP *v)
#else
amp_cmp (u, v)
     AMP *u;
     AMP *v;
#endif
{
  mp_size usize = u->size;
  mp_size vsize = v->size;
  mp_size size;
  mp_size i;
  mp_limb a, b;
  mp_srcptr up, vp;

  if (usize != vsize)
    return usize - vsize;

  if (usize == 0)
    return 0;

  size = abs (usize);

  up = u->d;
  vp = v->d;

  i = size - 1;
  do
    {
      a = up[i];
      b = vp[i];
      i--;
      if (i < 0)
	break;
    }
  while (a == b);

  if (a == b)
    return 0;

  if ((a < b) == (usize < 0))
    return 1;
  else
    return -1;
}

AMP*
#if __STDC__
amp_div (AMP *num, AMP *den)
#else
amp_div (num, den)
     AMP *num;
     AMP *den;
#endif
{
  AMP *quot;
  mp_srcptr np, dp;
  mp_ptr qp, rp;
  mp_size nsize = num->size;
  mp_size dsize = den->size;
  mp_size qsize, rsize;
  mp_size sign_quotient = nsize ^ dsize;
  unsigned normalization_steps;

  nsize = abs (nsize);
  dsize = abs (dsize);

  /* Ensure space is enough for quotient. */

  qsize = nsize - dsize + 1;	/* qsize cannot be bigger than this.  */
  if (qsize <= 0)
    {
      quot = amp_alloc (1);
      quot->size = 0;
      return quot;
    }

  quot = amp_alloc (qsize);

  qp = quot->d;
  np = num->d;
  dp = den->d;
  rp = (mp_ptr) alloca ((nsize + 1) * BYTES_PER_MP_LIMB);

  count_leading_zeros (normalization_steps, dp[dsize - 1]);

  /* Normalize the denominator and the numerator.  */
  if (normalization_steps != 0)
    {
      mp_ptr tp;
      mp_limb ndigit;

      /* Shift up the denominator setting the most significant bit of
	 the most significant word.  Use temporary storage not to clobber
	 the original contents of the denominator.  */
      tp = (mp_ptr) alloca (dsize * BYTES_PER_MP_LIMB);
      (void) _mpn_lshift (tp, dp, dsize, normalization_steps);
      dp = tp;

      /* Shift up the numerator, possibly introducing a new most
	 significant word.  Move the shifted numerator in the remainder
	 meanwhile.  */
      ndigit = _mpn_lshift (rp, np, nsize, normalization_steps);
      if (ndigit != 0)
	{
	  rp[nsize] = ndigit;
	  rsize = nsize + 1;
	}
      else
	rsize = nsize;
    }
  else
    {
      /* The denominator is already normalized, as required.
	 Copy it to temporary space if it overlaps with the quotient.  */
      if (dp == qp)
	{
	  dp = (mp_ptr) alloca (dsize * BYTES_PER_MP_LIMB);
	  MPN_COPY ((mp_ptr) dp, qp, dsize);
	}

      /* Move the numerator to the remainder.  */
      MPN_COPY (rp, np, nsize);
      rsize = nsize;
    }

  qsize = rsize - dsize + _mpn_div (qp, rp, rsize, dp, dsize);

  /* Normalize the quotient.  We may have at most one leading
     zero-word, so no loop is needed.  */
  if (qsize > 0)
    qsize -= (qp[qsize - 1] == 0);

  if (sign_quotient < 0)
    qsize = -qsize;
  quot->size = qsize;

  return quot;
}

AMP*
#if __STDC__
amp_divmod (AMP **quotp, AMP *num, AMP *den)
#else
amp_divmod (quotp, num, den)
     AMP **quotp;
     AMP *num;
     AMP *den;
#endif

#define COMPUTE_QUOTIENT
#define quot (*quotp)
#include "amp_dmincl.c"
/***********************/

AMP*
#if __STDC__
amp_from_int (signed long int val)
#else
amp_from_int (val)
     signed long int val;
#endif
{
  AMP *x;
  x = amp_alloc(1);
  if (val > 0)
    {
      x->d[0] = val;
      x->size = 1;
    }
  else if (val < 0)
    {
      x->d[0] = -val;
      x->size = -1;
    }
  else
    x->size = 0;
  return x;
}

int
#ifdef __STDC__
amp_to_int(AMP *x)
#else
amp_to_int(x)
    AMP *x;
#endif
{
  if (x->size > 0)
    return x->d[0];
  else if (x->size < 0)
    return -x->d[0];
  else
    return 0;
}

AMP*
#if __STDC__
amp_mod (AMP *num, AMP *den)
#else
amp_mod (num, den)
     AMP *num;
     AMP *den;
#endif

#undef COMPUTE_QUOTIENT
#include "amp_dmincl.c"
/**************************/

AMP*
#if __STDC__
amp_mul (AMP *u, AMP *v)
#else
amp_mul (u, v)
     AMP *u;
     AMP *v;
#endif
{
  AMP *w;
  mp_size usize = u->size;
  mp_size vsize = v->size;
  mp_size wsize;
  mp_size sign_product;

  sign_product = usize ^ vsize;
  usize = abs (usize);
  vsize = abs (vsize);

  if (usize < vsize)
    {
      /* Swap U and V.  */
      {AMP *t = u; u = v; v = t;}
      {mp_size t = usize; usize = vsize; vsize = t;}
    }

  /* Ensure W has space enough to store the result.  */
  wsize = usize + vsize;
  w = amp_alloc (wsize);

  wsize = _mpn_mul (w->d, u->d, usize, v->d, vsize);
  w->size = sign_product < 0 ? -wsize : wsize;
  return w;
}

AMP*
#if __STDC__
amp_neg (AMP *src)
#else
amp_neg (src)
     AMP *src;
#endif
{
  AMP *dst;
  mp_size src_size = src->size;
  mp_size abs_src_size = abs (src_size);

  dst = amp_alloc (abs_src_size);
  MPN_COPY (dst->d, src->d, abs_src_size);
  dst->size = -src_size;
  return dst;
}

AMP*
#if __STDC__
amp_sub (AMP *u, AMP *v)
#else
amp_sub (u, v)
     AMP *u;
     AMP *v;
#endif
{
  AMP *w;
  mp_srcptr up, vp;
  mp_ptr wp;
  mp_size usize, vsize, wsize;
  mp_size abs_usize;
  mp_size abs_vsize;

  usize = u->size;
  vsize = -v->size;		/* The "-" makes the difference from mpz_add */
  abs_usize = abs (usize);
  abs_vsize = abs (vsize);

  if (abs_usize < abs_vsize)
    {
      /* Swap U and V. */
      {AMP *t = u; u = v; v = t;}
      {mp_size t = usize; usize = vsize; vsize = t;}
      {mp_size t = abs_usize; abs_usize = abs_vsize; abs_vsize = t;}
    }

  /* True: abs(USIZE) >= abs(VSIZE) */

  /* If not space for sum (and possible carry), increase space.  */
  wsize = abs_usize + 1;
  w = amp_alloc (wsize);

  /* These must be after realloc (u or v may be the same as w).  */
  up = u->d;
  vp = v->d;
  wp = w->d;

  if (usize >= 0)
    {
      if (vsize >= 0)
	{
	  wsize = _mpn_add (wp, up, abs_usize, vp, abs_vsize);
	  if (wsize != 0)
	    wp[abs_usize] = 1;
	  wsize = wsize + abs_usize;
	}
      else
	{
	  /* The signs are different.  Need exact comparision to determine
	     which operand to subtract from which.  */
	  if (abs_usize == abs_vsize
	      && _mpn_cmp (up, abs_usize, vp, abs_usize) < 0)
	    wsize = -(abs_usize + _mpn_sub (wp, vp, abs_usize, up, abs_usize));
	  else
	    wsize = abs_usize + _mpn_sub (wp, up, abs_usize, vp, abs_vsize);
	}
    }
  else
    {
      if (vsize >= 0)
	{
	  /* The signs are different.  Need exact comparision to determine
	     which operand to subtract from which.  */
	  if (abs_usize == abs_vsize
	      && _mpn_cmp (up, abs_usize, vp, abs_usize) < 0)
	    wsize = abs_usize + _mpn_sub (wp, vp, abs_usize, up, abs_usize);
	  else
	    wsize = -(abs_usize + _mpn_sub (wp, up, abs_usize, vp, abs_vsize));
	}
      else
	{
	  wsize = _mpn_add (wp, up, abs_usize, vp, abs_vsize);
	  if (wsize != 0)
	    wp[abs_usize] = 1;
	  wsize = -(wsize + abs_usize);
	}
    }

  w->size = wsize;
  return w;
}


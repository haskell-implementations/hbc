/* mpz_ior -- Logical inclusive or.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "amp.h"

#define min(l,o) ((l) < (o) ? (l) : (o))
#define max(h,i) ((h) > (i) ? (h) : (i))

AMP*
#if __STDC__
amp_ior (AMP *op1, AMP *op2)
#else
amp_ior (op1, op2)
     AMP *op1;
     AMP *op2;
#endif
{
  AMP *res;
  mp_srcptr op1_ptr, op2_ptr;
  mp_size op1_size, op2_size;
  mp_ptr res_ptr;
  mp_size res_size;
  mp_size i;

  op1_size = op1->size;
  op2_size = op2->size;

  op1_ptr = op1->d;
  op2_ptr = op2->d;
  res_ptr = res->d;

  if (op1_size >= 0)
    {
      if (op2_size >= 0)
	{
	  if (op1_size >= op2_size)
	    {
/*	      if (res->alloc < op1_size)*/
		{
		  res = amp_alloc (op1_size);
		  op1_ptr = op1->d;
		  op2_ptr = op2->d;
		  res_ptr = res->d;
		}

	      if (res_ptr != op1_ptr)
		MPN_COPY (res_ptr + op2_size, op1_ptr + op2_size,
			  op1_size - op2_size);
	      for (i = op2_size - 1; i >= 0; i--)
		res_ptr[i] = op1_ptr[i] | op2_ptr[i];
	      res_size = op1_size;
	    }
	  else
	    {
/*	      if (res->alloc < op2_size)*/
		{
		  res = amp_alloc (op2_size);
		  op1_ptr = op1->d;
		  op2_ptr = op2->d;
		  res_ptr = res->d;
		}

	      if (res_ptr != op2_ptr)
		MPN_COPY (res_ptr + op1_size, op2_ptr + op1_size,
			  op2_size - op1_size);
	      for (i = op1_size - 1; i >= 0; i--)
		res_ptr[i] = op1_ptr[i] | op2_ptr[i];
	      res_size = op2_size;
	    }

	  res->size = res_size;
	  return res;
	}
      /*else  op2_size < 0 */
	/* Fall through to the code at the end of the function.  */
	/*;*/
    }
  else
    {
      if (op2_size < 0)
	{
	  mp_ptr opx;
	  mp_limb cy;
	  mp_limb one = 1;

	  /* Both operands are negative, so will be the result.
	     -((-OP1) | (-OP2)) = -(~(OP1 - 1) | ~(OP2 - 1)) =
	     = ~(~(OP1 - 1) | ~(OP2 - 1)) + 1 =
	     = ((OP1 - 1) & (OP2 - 1)) + 1      */

	  op1_size = -op1_size;
	  op2_size = -op2_size;

	  res_size = min (op1_size, op2_size);

	  /* Possible optimization: Decrease mpn_sub precision,
	     as we won't use the entire res of both.  */
	  opx = (mp_ptr) alloca (op1_size * BYTES_PER_MP_LIMB);
	  op1_size += _mpn_sub (opx, op1_ptr, op1_size, &one, 1);
	  op1_ptr = opx;

	  opx = (mp_ptr) alloca (op2_size * BYTES_PER_MP_LIMB);
	  op2_size += _mpn_sub (opx, op2_ptr, op2_size, &one, 1);
	  op2_ptr = opx;

/*	  if (res->alloc < res_size)*/
	    {
	      res = amp_alloc (res_size);
	      res_ptr = res->d;
	      /* Don't re-read OP1_PTR and OP2_PTR.  They point to
		 temporary space--never to the space RES->D used
		 to point to before reallocation.  */
	    }

	  /* First loop finds the size of the result.  */
	  for (i = res_size - 1; i >= 0; i--)
	    if ((op1_ptr[i] & op2_ptr[i]) != 0)
	      break;
	  res_size = i + 1;

	  /* Second loop computes the real result.  */
	  for (i = res_size - 1; i >= 0; i--)
	    res_ptr[i] = op1_ptr[i] & op2_ptr[i];

	  if (res_size != 0)
	    {
	      cy = _mpn_add (res_ptr, res_ptr, res_size, &one, 1);
	      if (cy)
		{
		  res_ptr[res_size] = cy;
		  res_size++;
		}
	    }
	  else
	    {
	      res_ptr[0] = 1;
	      res_size = 1;
	    }

	  res->size = -res_size;
	  return res;
	}
      else
	{
	  /* We should compute -OP1 | OP2.  Swap OP1 and OP2 and fall
	     through to the code that handles OP1 | -OP2.  */
	  {AMP *t = op1; op1 = op2; op2 = t;}
	  {mp_srcptr t = op1_ptr; op1_ptr = op2_ptr; op2_ptr = t;}
	  {mp_size t = op1_size; op1_size = op2_size; op2_size = t;}
	}
    }

  {
    mp_ptr opx;
    mp_limb cy;
    mp_limb one = 1;
    mp_size res_alloc;

    /* Operand 2 negative, so will be the result.
       -(OP1 | (-OP2)) = -(OP1 | ~(OP2 - 1)) =
       = ~(OP1 | ~(OP2 - 1)) + 1 =
       = (~OP1 & (OP2 - 1)) + 1      */

    op2_size = -op2_size;

    res_alloc = op2_size;

    opx = (mp_ptr) alloca (op2_size * BYTES_PER_MP_LIMB);
    op2_size += _mpn_sub (opx, op2_ptr, op2_size, &one, 1);
    op2_ptr = opx;

/*    if (res->alloc < res_alloc)*/
      {
	res = amp_alloc (res_alloc);
	op1_ptr = op1->d;
	res_ptr = res->d;
	/* Don't re-read OP2_PTR.  It points to temporary space--never
	   to the space RES->D used to point to before reallocation.  */
      }

    if (op1_size >= op2_size)
      {
	/* We can just ignore the part of OP1 that stretches above OP2,
	   because the result limbs are zero there.  */

	/* First loop finds the size of the result.  */
	for (i = op2_size - 1; i >= 0; i--)
	  if ((~op1_ptr[i] & op2_ptr[i]) != 0)
	    break;
	res_size = i + 1;
      }
    else
      {
	res_size = op2_size;

	/* Copy the part of OP2 that stretches above OP1, to RES.  */
	MPN_COPY (res_ptr + op1_size, op2_ptr + op1_size,
		  op2_size - op1_size);
      }

    /* Second loop computes the real result.  */
    for (i = res_size - 1; i >= 0; i--)
      res_ptr[i] = ~op1_ptr[i] & op2_ptr[i];

    if (res_size != 0)
      {
	cy = _mpn_add (res_ptr, res_ptr, res_size, &one, 1);
	if (cy)
	  {
	    res_ptr[res_size] = cy;
	    res_size++;
	  }
      }
    else
      {
	res_ptr[0] = 1;
	res_size = 1;
      }

    res->size = -res_size;
    return res;
  }
}

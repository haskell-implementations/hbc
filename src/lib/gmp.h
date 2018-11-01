/* gmp.h -- Definitions for GNU multiple precision functions.

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

#ifndef __GMP_H__
#define __GMP_H__

#define __GNU_MP__

#if !(defined(_AIX) || defined(hppa) || defined(__NetBSD__) || defined(__FreeBSD__) || defined(__CYGWIN32__)) || defined(OSF)
#define ALLOCADEF
#include <alloca.h>
#endif

#ifndef __MP_H__
#define __need_size_t
#include <stddef.h>
#endif

#ifndef MINT
#ifndef __MP_SMALL__
typedef struct
{
  long int alloc;		/* Number of *limbs* allocated and pointed
				   to by the D field.  */
  long int size;		/* abs(SIZE) is the number of limbs
				   the last field points to.  If SIZE
				   is negative this is a negative
				   number.  */
  unsigned long int *d;		/* Pointer to the limbs.  */
} __MP_INT;
#else
typedef struct
{
  short int alloc;		/* Number of *limbs* allocated and pointed
				   to by the D field.  */
  short int size;		/* abs(SIZE) is the number of limbs
				   the last field points to.  If SIZE
				   is negative this is a negative
				   number.  */
  unsigned long int *d;		/* Pointer to the limbs.  */
} __MP_INT;
#endif
#endif

#define MP_INT __MP_INT

typedef unsigned long int	mp_limb;
typedef long int		mp_limb_signed;
typedef mp_limb *		mp_ptr;
#ifdef __STDC__
typedef const mp_limb *		mp_srcptr;
#else
typedef mp_limb *		mp_srcptr;
#endif
typedef long int		mp_size;

/* Structure for rational numbers.  Zero is represented as 0/any, i.e.
   the denominator is ignored.  Negative numbers have the sign in
   the numerator.  */
typedef struct
{
  MP_INT num;
  MP_INT den;
#if 0
  long int num_alloc;		/* Number of limbs allocated
				   for the numerator.  */
  long int num_size;		/* The absolute value of this field is the
				   length of the numerator; the sign is the
				   sign of the entire rational number.  */
  mp_ptr num;			/* Pointer to the numerator limbs.  */
  long int den_alloc;		/* Number of limbs allocated
				   for the denominator.  */
  long int den_size;		/* Length of the denominator.  (This field
				   should always be positive.) */
  mp_ptr den;			/* Pointer to the denominator limbs.  */
#endif
} MP_RAT;

#ifdef __STDC__
void mp_set_memory_functions (void *(*alloc_func) (size_t),
			      void *(*realloc_func) (void *, size_t, size_t),
			      void (*free_func) (void *, size_t));

/**************** Integer (i.e. Z) routines.  ****************/

void mpz_init (MP_INT *);
void mpz_set (MP_INT *dest_integer, const MP_INT *src_integer);
void mpz_set_ui (MP_INT *integer, unsigned long int initial_value);
void mpz_set_si (MP_INT *integer, signed long int initial_value);
int mpz_set_str (MP_INT *integer, const char *initial_value, int base);
void mpz_init_set (MP_INT *dest_integer, const MP_INT *initial_value);
void mpz_init_set_ui (MP_INT *dest_integer, unsigned long int initial_value);
void mpz_init_set_si (MP_INT *dest_integer, signed long int initial_value);
int mpz_init_set_str (MP_INT *dest_integer, const char *initial_value,
		      int base);
unsigned long int mpz_get_ui (const MP_INT *src_integer);
signed long int mpz_get_si (const MP_INT *src_integer);
char * mpz_get_str (char *string, int base, const MP_INT *integer);
void mpz_clear (MP_INT *integer);
void * _mpz_realloc (MP_INT *integer, mp_size new_alloc);
void mpz_add (MP_INT *sum, const MP_INT *addend1, const MP_INT *addend2);
void mpz_add_ui (MP_INT *sum, const MP_INT *addend1,
		 unsigned long int addend2);
void mpz_sub (MP_INT *difference, const MP_INT *minuend,
	      const MP_INT *subtrahend);
void mpz_sub_ui (MP_INT *difference,
		 const MP_INT *minuend, unsigned long int subtrahend);
void mpz_mul (MP_INT *product,
	      const MP_INT *multiplicator, const MP_INT *multiplicand);
void mpz_mul_ui (MP_INT *product,
		 const MP_INT *multiplicator,
		 unsigned long int multiplicand);
void mpz_div (MP_INT *quotient, const MP_INT *dividend,
	      const MP_INT *divisor);
void mpz_div_ui (MP_INT *quotient,
		 const MP_INT *dividend, unsigned long int divisor);
void mpz_mod (MP_INT *remainder, const MP_INT *divdend,
	      const MP_INT *divisor);
void mpz_mod_ui (MP_INT *remainder,
		 const MP_INT *divdend, unsigned long int divisor);
void mpz_divmod (MP_INT *quotient, MP_INT *remainder,
		 const MP_INT *dividend, const MP_INT *divisor);
void mpz_divmod_ui (MP_INT *quotient, MP_INT *remainder,
		    const MP_INT *dividend, unsigned long int divisor);
void mpz_mdiv (MP_INT *quotient, const MP_INT *dividend,
	       const MP_INT *divisor);
void mpz_mdiv_ui (MP_INT *quotient,
		  const MP_INT *dividend, unsigned long int divisor);
void mpz_mmod (MP_INT *remainder, const MP_INT *divdend,
	       const MP_INT *divisor);
unsigned long int mpz_mmod_ui (MP_INT *remainder, const MP_INT *divdend,
			       unsigned long int divisor);
void mpz_mdivmod (MP_INT *quotient, MP_INT *remainder,
		  const MP_INT *dividend, const MP_INT *divisor);
unsigned long int mpz_mdivmod_ui (MP_INT *quotient, MP_INT *remainder,
				  const MP_INT *dividend,
				  unsigned long int divisor);
void mpz_sqrt (MP_INT *root, const MP_INT *operand);
void mpz_sqrtrem (MP_INT *root, MP_INT *remainder, const MP_INT *operand);
int mpz_perfect_square_p (const MP_INT *square);
int mpz_probab_prime_p (MP_INT *n, int reps);
void mpz_powm (MP_INT *res, const MP_INT *base, const MP_INT *exp,
	       const MP_INT *mod);
void mpz_powm_ui (MP_INT *res, const MP_INT *base, unsigned long int exp,
		  const MP_INT *mod);
void mpz_pow_ui (MP_INT *res, const MP_INT *base, unsigned long int exp);
void mpz_fac_ui (MP_INT *res, unsigned long int exp);
void mpz_gcd (MP_INT *res, const MP_INT *operand1, const MP_INT *operand2);
void mpz_gcdext (MP_INT *g, MP_INT *s, MP_INT *t,
		 const MP_INT *a, const MP_INT *b);
void mpz_neg (MP_INT *negated_operand, const MP_INT *operand);
void mpz_abs (MP_INT *positive_operand, const MP_INT *signed_operand);
int mpz_cmp (const MP_INT *operand1, const MP_INT *operand2);
int mpz_cmp_ui (const MP_INT *operand1, unsigned long int operand2);
int mpz_cmp_si (const MP_INT *operand1, signed long int operand2);
void mpz_mul_2exp (MP_INT *product, const MP_INT *multiplicator,
		   unsigned long int exponent_of_2);
void mpz_div_2exp (MP_INT *quotient,
		   const MP_INT *dividend, unsigned long int exponent_of_2);
void mpz_mod_2exp (MP_INT *remainder,
		   const MP_INT *dividend, unsigned long int exponent_of_2);
void mpz_and (MP_INT *conjunction, const MP_INT *operand1,
	      const MP_INT *operand2);
void mpz_ior (MP_INT *disjunction, const MP_INT *operand1,
	      const MP_INT *operand2);
void mpz_xor (MP_INT *disjunction, const MP_INT *operand1,
	      const MP_INT *operand2);
void mpz_not (MP_INT *complemented_operand, const MP_INT *operand);

#ifdef FILE
void mpz_inp_raw (MP_INT *integer, FILE *stream);
void mpz_inp_str (MP_INT *integer, FILE *stream, int base);
void mpz_out_raw (FILE *stream, const MP_INT *integer);
void mpz_out_str (FILE *stream, int base, const MP_INT *integer);
#endif

void mpz_array_init (MP_INT integer_array[],
		     size_t array_size, mp_size fixed_num_limbs);
void mpz_random (MP_INT *random_integer, mp_size max_size);
void mpz_random2 (MP_INT *random_integer, mp_size max_size);
size_t mpz_size (const MP_INT *integer);
size_t mpz_sizeinbase (const MP_INT *integer, int base);

/**************** Rational (i.e. Q) routines.  ****************/

void mpq_init (MP_RAT *);
void mpq_clear (MP_RAT *);
void mpq_set (MP_RAT *dest_rational, const MP_RAT *src_rational);
void mpq_set_ui (MP_RAT *rational_number,
		 unsigned long int numerator, unsigned long int denominator);
void mpq_set_si (MP_RAT *rational_number,
		 signed long int numerator, unsigned long int denominator);
void mpq_add (MP_RAT *sum, const MP_RAT *addend1, const MP_RAT *addend2);
void mpq_sub (MP_RAT *difference, const MP_RAT *minuend,
	      const MP_RAT *subtrahend);
void mpq_mul (MP_RAT *product,
	      const MP_RAT *multiplicator, const MP_RAT *multiplicand);
void mpq_div (MP_RAT *quotient, const MP_RAT *dividend,
	      const MP_RAT *divisor);
void mpq_neg (MP_RAT *negated_number, const MP_RAT *number);
int mpq_cmp (const MP_RAT *operand1, const MP_RAT *operand2);
void mpq_inv (MP_RAT *inverted_number, const MP_RAT *number);
void mpq_set_num (MP_RAT *rational_number, const MP_INT *numerator);
void mpq_set_den (MP_RAT *rational_number, const MP_INT *denominator);
void mpq_get_num (MP_INT *numerator, const MP_RAT *rational_number);
void mpq_get_den (MP_INT *denominator, const MP_RAT *rational_number);

/************ Low level positive-integer (i.e. N) routines.  ************/

mp_size _mpn_add (mp_ptr, mp_srcptr, mp_size, mp_srcptr, mp_size);
mp_size _mpn_sub (mp_ptr, mp_srcptr, mp_size, mp_srcptr, mp_size);
mp_size _mpn_mul (mp_ptr, mp_srcptr, mp_size, mp_srcptr, mp_size);
mp_size _mpn_div (mp_ptr, mp_ptr, mp_size, mp_srcptr, mp_size);
mp_limb _mpn_lshift (mp_ptr, mp_srcptr, mp_size, unsigned long int);
mp_size _mpn_rshift (mp_ptr, mp_srcptr, mp_size, unsigned long int);
mp_size _mpn_rshiftci (mp_ptr, mp_srcptr, mp_size,unsigned long int,mp_limb);
int _mpn_cmp (mp_srcptr, mp_size, mp_srcptr, mp_size);

#else /* ! __STDC__ */
void mp_set_memory_functions ();

/**************** Integer (i.e. Z) routines.  ****************/

void mpz_init ();
void mpz_set ();
void mpz_set_ui ();
void mpz_set_si ();
int mpz_set_str ();
void mpz_init_set ();
void mpz_init_set_ui ();
void mpz_init_set_si ();
int mpz_init_set_str ();
unsigned long int mpz_get_ui ();
long int mpz_get_si ();
char * mpz_get_str ();
void mpz_clear ();
void * _mpz_realloc ();
void mpz_add ();
void mpz_add_ui ();
void mpz_sub ();
void mpz_sub_ui ();
void mpz_mul ();
void mpz_mul_ui ();
void mpz_div ();
void mpz_div_ui ();
void mpz_mod ();
void mpz_mod_ui ();
void mpz_divmod ();
void mpz_divmod_ui ();
void mpz_mdiv ();
void mpz_mdiv_ui ();
void mpz_mmod ();
unsigned long int mpz_mmod_ui ();
void mpz_mdivmod ();
unsigned long int mpz_mdivmod_ui ();
void mpz_sqrt ();
void mpz_sqrtrem ();
int mpz_perfect_square_p ();
int mpz_probab_prime_p ();
void mpz_powm ();
void mpz_powm_ui ();
void mpz_pow_ui ();
void mpz_fac_ui ();
void mpz_gcd ();
void mpz_gcdext ();
void mpz_neg ();
void mpz_abs ();
int mpz_cmp ();
int mpz_cmp_ui ();
int mpz_cmp_si ();
void mpz_mul_2exp ();
void mpz_div_2exp ();
void mpz_mod_2exp ();
void mpz_and ();
void mpz_ior ();
void mpz_xor ();
void mpz_not ();

#ifdef FILE
void mpz_inp_raw ();
void mpz_inp_str ();
void mpz_out_raw ();
void mpz_out_str ();
#endif

void mpz_array_init ();
void mpz_random ();
void mpz_random2 ();
size_t mpz_size ();
size_t mpz_sizeinbase ();

/**************** Rational (i.e. Q) routines.  ****************/

void mpq_init ();
void mpq_clear ();
void mpq_set ();
void mpq_set_ui ();
void mpq_set_si ();
void mpq_add ();
void mpq_sub ();
void mpq_mul ();
void mpq_div ();
void mpq_neg ();
int mpq_cmp ();
void mpq_inv ();
void mpq_set_num ();
void mpq_set_den ();
void mpq_get_num ();
void mpq_get_den ();

/************ Low level positive-integer (i.e. N) routines.  ************/

mp_size _mpn_add ();
mp_size _mpn_sub ();
mp_size _mpn_mul ();
mp_size _mpn_div ();
mp_limb _mpn_lshift ();
mp_size _mpn_rshift ();
mp_size _mpn_rshiftci ();
int _mpn_cmp ();
#endif /* __STDC__ */

#endif /* __GMP_H__ */

/* gmp-mparam.h -- Compiler/machine parameter header file.

   ***** THIS FILE WAS CREATED BY A PROGRAM.  DON'T EDIT IT! *****

Copyright (C) 1991 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2, or
(at your option) any later version.

The GNU MP Library is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty
of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with the GNU MP Library; see the file COPYING.  If not, write
to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139,
USA.  */

#if defined(__alpha)

#define BITS_PER_MP_LIMB 64
#define BYTES_PER_MP_LIMB 8
#define BITS_PER_LONGINT 64
#define BITS_PER_INT 32
#define BITS_PER_SHORTINT 16
#define BITS_PER_CHAR 8

#define LONG_TYPE_SIZE 64

#else

#define BITS_PER_MP_LIMB 32
#define BYTES_PER_MP_LIMB 4
#define BITS_PER_LONGINT 32
#define BITS_PER_INT 32
#define BITS_PER_SHORTINT 16
#define BITS_PER_CHAR 8

#endif

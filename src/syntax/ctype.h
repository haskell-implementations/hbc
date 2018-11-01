#ifndef _CTYPE_H_
#define _CTYPE_H_


#define	_U	0001	/* Upper case */
#define	_L	0002	/* Lower case */
#define	_N	0004	/* Numeral (digit) */
#define	_S	0010	/* Spacing character */
#define _P	0020	/* Punctuation */
#define _C	0040	/* Control character */
#define _X	0100	/* Hexadecimal */
#define _B	0200	/* Blank */

extern  unsigned short _ctype__[];
extern	unsigned short *_pctype;

#ifdef __STDC__
/*
 *  prototype
 *
 */
int	isalnum( int __c );
int	isalpha( int __c );
int	isascii( int __c );
int	iscntrl( int __c );
int	isdigit( int __c );
int	isgraph( int __c );
int	islower( int __c );
int	isprint( int __c );
int	ispunct( int __c );
int	isspace( int __c );
int	isupper( int __c );
int	isxdigit( int __c );
int	toascii( int __c );
int	_tolower( int __c );
int	_toupper( int __c );
int	tolower( int __c );
int	toupper( int __c );
#else
extern	int	tolower(), toupper();
#endif /* __STDC__ */

#define	isalpha(c)	((_pctype+1)[c]&(_U|_L))
#define	isupper(c)	((_pctype+1)[c]&_U)
#define	islower(c)	((_pctype+1)[c]&_L)
#define	isdigit(c)	((_ctype__+1)[c]&_N)
#define	isxdigit(c)	((_ctype__+1)[c]&(_N|_X))
#define	isspace(c)	((_pctype+1)[c]&_S)
#define ispunct(c)	((_pctype+1)[c]&_P)
#define isalnum(c)	((_pctype+1)[c]&(_U|_L|_N))
#define isprint(c)	((_pctype+1)[c]&(_P|_U|_L|_N|_B))
#define isgraph(c)	((_pctype+1)[c]&(_P|_U|_L|_N))
#define iscntrl(c)	((_pctype+1)[c]&_C)
#define isascii(c)	((unsigned)(c)<=0177)
#if !defined(_POSIX_SOURCE) || defined(_XOPEN_SOURCE)
#define _toupper(c)	((c)-'a'+'A')
#define _tolower(c)	((c)-'A'+'a')
#define toascii(c)	((c)&0177)
#endif

#endif /* _CTYPE_H_ */

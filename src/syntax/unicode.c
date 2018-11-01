#include <sys/types.h>

struct UnicodeInfo {
  int version;
  int lowerOffs[64];
  int upperOffs[64];
  unsigned char info[65536];
};
static struct UnicodeInfo uniinfo;

/* info encoding:
   000ttttt -  ttttt encodes the character type from GeneralCategory
   001ddddd -  digit with ddddd as the decoding method
   10BBBBBB -  lower case, BBBBBB is used to index upperOffs 
                           to get the delta to the upper case
                           equivalent char (or NOEQUIV if none
                           exists).
   11BBBBBB -  upper case, BBBBBB used with lowerOffs.
*/

#define VERSION 19970220
#define NOEQUIV 0x80000000

enum GeneralCategory {
  GC_None,
  GC_Mn, GC_Mc, GC_Me, GC_Nd,
  GC_Nl, GC_No, GC_Zs, GC_Zl,
  GC_Zp, GC_Cc, GC_Cf, GC_Cs,
  GC_Co, GC_Cn, GC_Lu, GC_Ll,
  GC_Lt, GC_Lm, GC_Lo, GC_Pc,
  GC_Pd, GC_Ps, GC_Pe, GC_Po,
  GC_Sm, GC_Sc, GC_Sk, GC_So,
};

#define D_NONE  0
#define D_OFFS0 1
#define D_OFFS6 2

#define	CT_DIGIT	0x01
#define CT_ALPHA	0x02
#define CT_SYMBOL	0x04
#define CT_SPACE	0x08
/* indexed by GeneralCategory */
int ctype[] = {
  0,
  0, 		0, 		0,		CT_DIGIT,
  0, 		0, 		CT_SPACE,	0,
  0,		0,		0,		0,
  0,		0,		0/*upper*/,	0/*lower*/,
  CT_ALPHA,	0,		CT_ALPHA,	0,
  CT_SYMBOL,	CT_SYMBOL,	CT_SYMBOL,	CT_SYMBOL,
  CT_SYMBOL,	CT_SYMBOL,	CT_SYMBOL,	CT_SYMBOL,
};

int 
uni_islower(c)
int c;
{
  return c >= 0 && (uniinfo.info[c] & 0xc0) == 0x80;
}

int 
uni_isupper(c)
int c;
{
  return c >= 0 && (uniinfo.info[c] & 0xc0) == 0xc0;
}

int 
uni_isalpha(c)
int c;
{
  int i = uniinfo.info[c];
  return c >= 0 && 
    ((i & 0x80) == 0x80 ||
     (ctype[i & 0x1f] & CT_ALPHA) != 0);
}

int
uni_isdigit(c)
int c;
{
  return c >= 0 && (uniinfo.info[c] & 0xe0) == 0x20;
}

int
uni_issymbol(c)
int c;
{
  int i = uniinfo.info[c];
  if (c >= 0 && (i & 0xe0) == 0x00)
    return (ctype[i & 0x1f] & CT_SYMBOL) != 0;
  else
    return 0;
}

int
uni_toupper(c)
int c;
{
  int i = uniinfo.info[c];
  if (c >= 0 && (i & 0xc0) == 0x80) {
    int o = uniinfo.upperOffs[i & 0x3f];
    if (o != NOEQUIV)
      return c + o;
    else
      return c;
  } else
    return c;
}

int
uni_tolower(c)
int c;
{
  int i = uniinfo.info[c];
  if (c >= 0 && (i & 0xc0) == 0xc0) {
    int o = uniinfo.lowerOffs[i & 0x3f];
    if (o != NOEQUIV)
      return c + o;
    else
      return c;
  } else
    return c;
}

int
uni_digitvalue(c)
int c;
{
  int i = uniinfo.info[c];
  if (c >= 0 && (i & 0xe0) == 0x20) {
    switch(i & 0x1f) {
    case D_OFFS0:
      return c & 0xf;
    case D_OFFS6:
      return (c-6) & 0xf;
    default:
      return -1;
    }
  } else
    return -1;
}

static long
xntohl(l)
long l;
{
  union { unsigned char c[4]; long l; } u;
  u.l = l;
  return (u.c[0] << 24) | (u.c[1] << 16) | (u.c[2] << 8) | u.c[3];
}

int
readUniFile(name)
char *name;
{
  int fd;
  int r;
  int i;

  fd = open(name, 0);
  if (fd < 0)
    return -1;
  r = read(fd, &uniinfo, sizeof uniinfo);
  close(fd);
  if (r != sizeof uniinfo)
    return -1;
  uniinfo.version = xntohl(uniinfo.version);
  for(i = 0; i < 64; i++) {
    uniinfo.lowerOffs[i] = xntohl(uniinfo.lowerOffs[i]);
    uniinfo.upperOffs[i] = xntohl(uniinfo.upperOffs[i]);
  }
  return 0;
}

#define AND {-:"Pand":-}
#define OR  {-:"Por":-}
#define XOR {-:"Pxor":-}
#define COMPL {-:"Pcompl":-}
#define LSH {-:"Plsh":-}
#define RSH {-:"Prsh":-}
#define RSHA {-:"Prsha":-}
#define IAND {-:"PIntegerAnd":-}
#define IOR {-:"PIntegerOr":-}
#define ICOMPL(x) (-(x)-1)

#ifdef __alpha
#define INT64 Int

#define TOUNSIGNED64(x) (if x >= 0 then toInteger x else 0x10000000000000000 + toInteger x)
#define TOUNSIGNED32(x) (x)
#define FROMINTEGER32(x) (x)

#define MASK64(x) (x)
#define SEXT64(x) (x)

#define MASK32(x) (AND (x) 0xffffffff)
#define SEXT32(x) (if AND (x) 0x80000000 /= 0 then  (x) - 0x100000000 else (x))

#else
#define INT64 Integer

#define TOUNSIGNED64(x) (x)
#define TOUNSIGNED32(x) (if x >= 0 then toInteger x else 0x100000000 + toInteger x)
#define FROMINTEGER32(x) (fromInteger x)

#define MASK64(x) ((x) `mod` 0x10000000000000000)
#define SEXT64(x) (x)

#define MASK32(x) (x)
#define SEXT32(x) (x)
#endif

#define MASK16(x) (AND (x) 0xffff)
#define SEXT16(x) (if AND (x) 0x8000 /= 0 then (x) - 0x10000 else (x))

#define MASK8(x) (AND (x) 0xff)
#define SEXT8(x) (if AND (x) 0x80 /= 0 then (x) - 0x100 else (x))

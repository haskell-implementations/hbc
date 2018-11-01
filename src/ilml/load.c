#include "../runtime/node.h"	/* to get function prototypes */

#if defined(mips) || defined(__osf__)
#define nlist __nlist
#endif

#include <a.out.h>

#if defined(mips) || defined(__osf__)
#undef nlist
#endif

int divi PROTO((int,int));
#ifdef DEBUG
static void dprintf();
#endif
static void error PROTO((char *, char *));

char *load_errmsg = 0;

#if defined(mips)
#ifdef irix
#include <sys/cachectl.h>
#else
#include <mips/cachectl.h>
#endif
#endif

#if defined(mips) || defined(__osf__)
#define MIPSCOFF 1
#define EXEC AOUTHDR
static struct filehdr filehdr; 
static struct scnhdr thdr, dhdr, rdhdr;
static int nlocals;
static int globgp;
static int oldvalue;
#define TXT_OFF(x) thdr.s_scnptr
#define DAT_OFF(x) dhdr.s_scnptr
#define RDAT_OFF(x) rdhdr.s_scnptr

#define relocation_info reloc
#define RELOC_ADDRESS(r)		((r)->r_vaddr)
#define RELOC_EXTERN_P(r)               ((r)->r_extern)      
#define RELOC_TYPE(r)                   (reloctype(r))
#define RELOC_SYMBOL(r)                 ((r)->r_symndx + nlocals)
#define RELOC_MEMORY_SUB_P(r)		0
#define RELOC_MEMORY_ADD_P(r)           ((r)->r_type != R_REFHI && (r)->r_type != R_REFLO)
#define RELOC_PCREL_P(r)                0
#define RELOC_VALUE_RIGHTSHIFT(r)       (reloc_target_rightshift[(r)->r_type])
#define RELOC_TARGET_SIZE(r)            (reloc_target_size[(r)->r_type])
#define RELOC_TARGET_BITPOS(r)          0
#define RELOC_TARGET_BITSIZE(r)         (reloc_target_bitsize[(r)->r_type])
#define RELOC_ADD_EXTRA(r)		(r->r_type == R_GPREL ? -globgp : oldvalue)
#define RELOC_32_ADJUST(r,x)		if (r->r_type == R_REFHI && (x & 0x8000)) x += 0x10000

static int reloc_target_size[] = {
0, 1,  2,  2,  2,  2,  2,  2,
};
static int reloc_target_bitsize[] = {
0, 16, 32, 26, 16, 16, 16, 16,
};
static int reloc_target_rightshift[] = {
0, 0,  0,  2,  16,  0,  0,  0,
};

struct nlist {
        union {
                char *n_name;           /* for use when in-core */
                long n_strx;            /* index into file string table */
        } n_un;
        unsigned char n_type;           /* type flag, N_TEXT etc; see below */
        char    n_other;                /* unused */
        short   n_desc;
        unsigned long   n_value;        /* value of symbol (or sdb offset) */
};
#undef N_UNDF
#define N_UNDF          0x00            /* undefined */
#define N_ABS           0x02            /* absolute */
#define N_TEXT          0x04            /* text (implicitly shared) */
#define N_DATA          0x06            /* private data */
#define N_BSS           0x08            /* private bss */
#define N_COMM          0x0a            /* common (internal to ld) */
#define N_EXT           0x01            /* external bit, or'ed in */
#define N_TYPE          0x1e            /* mask for all the type bits */

#define N_SUNDF		0x20

#define a_magic magic
#define a_text tsize
#define a_data dsize
#define a_bss bsize

#define TRELSIZE(x) ((thdr.s_flags & S_NRELOC_OVFL ? getrel(fi) : thdr.s_nreloc)*sizeof(struct reloc))
#define DRELSIZE(x) ((dhdr.s_flags & S_NRELOC_OVFL ? getrel(fi) : dhdr.s_nreloc)*sizeof(struct reloc))
#define N_TROFF(x)  (thdr.s_relptr)
#define N_DROFF(x)  (dhdr.s_relptr)
#define N_RDROFF(x)  (rdhdr.s_relptr)

static int
getrel(f)
FILE *f;
{
    struct reloc r;
    fread(&r, sizeof(struct reloc), 1, f);
    if (r.r_type != R_ABS) {
	fprintf(stderr, "No R_ABS\n");
	exit(1);
    }
    return r.r_vaddr-1;
}   

static int
reloctype(r)
struct reloc *r;
{
    switch(r->r_symndx) {
    case R_SN_TEXT:	return N_TEXT;
    case R_SN_RDATA:    return N_DATA;
    case R_SN_DATA:	return N_DATA;
    default:
	fprintf(stderr, "bad reloctype %d\n", r->r_symndx);
	exit(1);
    }
}

static int
convert(sc, st)
int sc, st;
{
    switch(sc) {
    case scNil: 	return N_UNDF;
    case scText:	return N_TEXT;
    case scData:	return N_DATA;
    case scUndefined:	return N_UNDF;
    case scSUndefined:	return N_SUNDF;
    default:
	fprintf(stderr, "Strange symbol %d %d\n", sc, st);
	exit(1);
    }
}

#else
#define MIPSCOFF 0
#endif

#if defined(sun) && defined(sparc)
/* Sparc (Sun 4) macros */
#undef relocation_info
#define relocation_info	                reloc_info_sparc
#define RELOC_ADDRESS(r)		((r)->r_address)                 
#define RELOC_EXTERN_P(r)               ((r)->r_extern)      
#define RELOC_TYPE(r)                   ((r)->r_index)  
#define RELOC_SYMBOL(r)                 ((r)->r_index)   
#define RELOC_MEMORY_SUB_P(r)		0
#define RELOC_MEMORY_ADD_P(r)           0
#define RELOC_ADD_EXTRA(r)              ((r)->r_addend)       
#define RELOC_PCREL_P(r)             \
        ((r)->r_type >= RELOC_DISP8 && (r)->r_type <= RELOC_WDISP22)
#define RELOC_VALUE_RIGHTSHIFT(r)       (reloc_target_rightshift[(r)->r_type])
#define RELOC_TARGET_SIZE(r)            (reloc_target_size[(r)->r_type])
#define RELOC_TARGET_BITPOS(r)          0
#define RELOC_TARGET_BITSIZE(r)         (reloc_target_bitsize[(r)->r_type])
#define DO_PCREL			1

/* Note that these are very dependent on the order of the enums in
   enum reloc_type (in a.out.h); if they change the following must be
   changed */
/* Also note that the last few may be incorrect; I have no information */
static int reloc_target_rightshift[] = {
  0, 0,   0, 0,  0,  0,  2,  2, 10,  0,  0,  0,  0,  0,  0,
};
static int reloc_target_size[] = {
  0, 1,   2, 0,  1,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
};
static int reloc_target_bitsize[] = {
  8, 16, 32, 8, 16, 32, 30, 22, 22, 22, 13, 10, 32, 32, 16,
};

#define	MAX_ALIGNMENT	(sizeof (double))
#endif

#ifdef sequent
#define RELOC_ADDRESS(r)		((r)->r_address)
#define RELOC_EXTERN_P(r)		((r)->r_extern)
#define RELOC_TYPE(r)		((r)->r_symbolnum)
#define RELOC_SYMBOL(r)		((r)->r_symbolnum)
#define RELOC_MEMORY_SUB_P(r)	((r)->r_bsr)
#define RELOC_MEMORY_ADD_P(r)	1
#undef RELOC_ADD_EXTRA
#define RELOC_PCREL_P(r)		((r)->r_pcrel || (r)->r_bsr)
#define RELOC_VALUE_RIGHTSHIFT(r)	0
#define RELOC_TARGET_SIZE(r)		((r)->r_length)
#define RELOC_TARGET_BITPOS(r)	0
#define RELOC_TARGET_BITSIZE(r)	32
#define RELOC_DISP(r)		((r)->r_disp)
#endif

/* Default macros */
#ifndef RELOC_ADDRESS
#define RELOC_ADDRESS(r)		((r)->r_address)
#define RELOC_EXTERN_P(r)		((r)->r_extern)
#define RELOC_TYPE(r)		((r)->r_symbolnum)
#define RELOC_SYMBOL(r)		((r)->r_symbolnum)
#define RELOC_MEMORY_SUB_P(r)	0
#define RELOC_MEMORY_ADD_P(r)	1
#undef RELOC_ADD_EXTRA
#define RELOC_PCREL_P(r)		((r)->r_pcrel)
#define RELOC_VALUE_RIGHTSHIFT(r)	0
#define RELOC_TARGET_SIZE(r)		((r)->r_length)
#define RELOC_TARGET_BITPOS(r)	0
#define RELOC_TARGET_BITSIZE(r)	32
#endif

#ifndef MAX_ALIGNMENT
#define	MAX_ALIGNMENT	(sizeof (int))
#endif

#ifndef RELOC_DISP
#define RELOC_DISP(r) 0
#endif
#ifndef DO_PCREL
#define DO_PCREL 0
#endif

#ifndef EXEC
#define EXEC struct exec
#endif

#ifdef sun
#define N_DATAOFF(x) N_DATOFF(x)
#define N_TROFF(x) N_TRELOFF(x)
#define N_DROFF(x) N_DRELOFF(x)
#endif

#if defined(__386BSD__) || defined(linux) || defined(__NetBSD__) || defined(__FreeBSD__)
#define N_DATAOFF(x)    (N_TXTOFF(x) + (x).a_text)
#define N_TROFF(x)      (N_DATAOFF(x) + (x).a_data)
#define N_DROFF(x)      (N_TROFF(x) + (x).a_trsize)
#endif

#ifdef vax
#define N_DATAOFF(x)    (N_TXTOFF(x) + (x).a_text)
#define N_TROFF(x)      (N_DATAOFF(x) + (x).a_data)
#define N_DROFF(x)      (N_TROFF(x) + (x).a_trsize)
#endif

#ifndef TXT_OFF
#define TXT_OFF(x) N_TXTOFF(x)
#define DAT_OFF(x) N_DATAOFF(x)
#endif

#ifndef TRELSIZE
#define TRELSIZE(x) (x)->a_trsize
#define DRELSIZE(x) (x)->a_drsize
#endif


/*static*/extern int debug;
typedef unsigned long coreaddr;

struct gtab {
    struct gtab *next;
    coreaddr value;
    struct relocation_info reloc;
    coreaddr pcr;
    int claimed;
    char name[1];
};

#define NHASH 255
static struct gtab *globtabs[NHASH], *undeftabs[NHASH];

extern struct {
    char *n;
    coreaddr l;
} predefs[];

static void
setval(p, v, rp, pcr)
coreaddr p, v, pcr;
struct relocation_info *rp;
{
    coreaddr mask;

#ifdef DEBUG
    if (debug > 2)
	dprintf("setval-%d v=%08x ", RELOC_TARGET_SIZE(rp), v);
#endif

#if MIPSCOFF
    if (rp->r_type == R_JMPADDR && (v&0x0fffffff) != v) {
	fprintf(stderr, "Cannot relocate jump target %x.\n", v);
	exit(1);
    }
#endif

    if (RELOC_PCREL_P(rp))
	v -= pcr;
#ifdef RELOC_ADD_EXTRA
    v += RELOC_ADD_EXTRA(rp);
#endif
#ifdef RELOC_32_ADJUST
    RELOC_32_ADJUST(rp,v);
#endif
    v >>= RELOC_VALUE_RIGHTSHIFT(rp);

    /* Unshifted mask for relocation */
    mask = 1 << (RELOC_TARGET_BITSIZE(rp) - 1);
    mask |= mask - 1;
#if defined(sparc)
    if (rp->r_type == RELOC_WDISP22) {
	/* check for jump offset overflow */
	coreaddr x;
	x = v & 0x3fe00000;	/* keep the 8 bits masked off + the sign bit */
	if (x != 0 && x != 0x3fe00000) {
	    {
		unsigned long brslot, nopslot, extraslot;
		unsigned long sethi, jmp;
		brslot = ((unsigned long *)p)[0];
		nopslot = ((unsigned long *)p)[1];
		extraslot = ((unsigned long *)p)[2];
		if (extraslot == 0x12344321) {
		    /* a magic extra slot has been reserved for us, use it */
		    v <<= 2;
		    v += p;	/* the real jump target */
		    sethi = 0x3b000000 + ((v>>10) & 0x3fffff);
		    jmp = 0x81c76000 + (v & 0x3ff);
		    ((unsigned long *)p)[0] = sethi;
		    ((unsigned long *)p)[1] = jmp;
		    ((unsigned long *)p)[2] = nopslot;
		    if (debug > 1)
			fprintf(stderr, "Whizzy relocation %x %x %x.\n", sethi, jmp, nopslot);
		    return;
		} else {
		    fprintf(stderr, "Cannot relocate jump target %x (mask=%x).\n", v, mask);
		    exit(1);
		}
	    }
	}
    }
#endif
    v &= mask;

    /* Shift everything up to where it's going to be used */
    v <<= RELOC_TARGET_BITPOS(rp);
    mask <<= RELOC_TARGET_BITPOS(rp);

#ifdef DEBUG
    if (debug > 2)
	dprintf("v'=%08x mask=%08x %08x:%08x->", v, mask, p, *(long *)p);
#endif

    switch (RELOC_TARGET_SIZE(rp)) {
    case 0:
	if (RELOC_MEMORY_SUB_P(rp))
	    v -= mask & *(char *) p;
	else if (RELOC_MEMORY_ADD_P(rp))
	    v += mask & *(char *) p;
	*(char *) p &= ~mask;
	*(char *) p |= v;
	break;

    case 1:
	if (RELOC_MEMORY_SUB_P(rp))
	    v -= mask & *(short *) p;
	else if (RELOC_MEMORY_ADD_P(rp))
	    v += mask & *(short *) p;
	*(short *) p &= ~mask;
	*(short *) p |= v;
	break;
	
    case 2:
	if (RELOC_MEMORY_SUB_P(rp))
	    v -= mask & *(long *) p;
	else if (RELOC_MEMORY_ADD_P(rp))
	    v += mask & *(long *) p;
	*(long *) p &= ~mask;
	*(long *) p |= v;
	break;

    default:
	error("Unimplemented relocation", "");
    }
#ifdef DEBUG
    if (debug > 2)
	dprintf("%08x\n", *(long *)p);
#endif
}

static int
hash(s)
register char *s;
{
    register int r;
    for(r = 0; *s; s++)
	r = r + r + r + *s;
    if (r < 0)
	r = -r;
    return r%NHASH;
}

static
struct gtab *
entry(name, value, next)
char *name;
coreaddr value;
struct gtab *next;
{
    struct gtab *p;

    p = (struct gtab*)xmalloc(sizeof (struct gtab) + strlen(name));
    p->next = next;
    p->value = value;
    p->claimed = 0;
    strcpy(p->name, name);
    return p;
}

static coreaddr
ilookup(name)
char *name;
{
    struct gtab *p;

#ifdef DEBUG
    if (debug > 1)
	dprintf("looking for %-25s...", name);
#endif /*DEBUG*/
    for (p = globtabs[hash(name)]; p; p = p->next) {
	if (strcmp(p->name, name) == 0) {
#ifdef DEBUG
	    if (debug > 1)
		dprintf("found %08x\n", p->value);
#endif /*DEBUG*/
	    return p->value;
	}
    }
#ifdef DEBUG
    if (debug > 1)
	dprintf("not found\n");
#endif /*DEBUG*/
    return 0;
}

void
scantab(f)
int (*f)();
{
    struct gtab *p;
    int i;

    for(i = 0; i < NHASH; i++) {
	for(p = globtabs[i]; p; p = p->next) {
	    if (!p->claimed) {
		(*f)(p->name, p->value);
		p->claimed = 1;
	    }
	}
    }
}

static void
addundef(name, value, rp, pcr)
char *name;
coreaddr value, pcr;
struct relocation_info *rp;
{
    struct gtab **t = &undeftabs[hash(name)];
#ifdef DEBUG
    if (debug > 1)
	dprintf("addundef %-25s %08x\n", name, value);
#endif /*DEBUG*/
    *t = entry(name, value, *t);
    (*t)->reloc = *rp;
    (*t)->pcr = pcr;
}

static void
addglob(name, value)
char *name;
coreaddr value;
{
    struct gtab **p;
    struct gtab **t = &globtabs[hash(name)];

#ifdef DEBUG
    if (debug > 1)
	dprintf("addglob %-25s %08x\n", name, value);
#endif /*DEBUG*/

    for(p = &undeftabs[hash(name)]; *p; ) {
	if (strcmp(name, (*p)->name) == 0) {
#ifdef DEBUG
	    if (debug > 1)
		dprintf("resolve %-25s %08x to %08x\n", (*p)->name, (*p)->value, value);
#endif /*DEBUG*/
	    setval((*p)->value, value, &(*p)->reloc, (*p)->pcr);
	    *p = (*p)->next;
	} else
	    p = &(*p)->next;
    }
    *t = entry(name, value, *t);
}

static void
inittab()
{
    int i;

#if MIPSCOFF
    {
	extern int getgp();
	globgp = getgp();
    }
#endif
    for(i = 0; predefs[i].n; i++)
	addglob(predefs[i].n, predefs[i].l);
}

#if 0
static void
printundef()
{
    struct gtab *p;
    int i;

    for(i = 0; i < NHASH; i++)
	for(p = undeftabs[i]; p; p = p->next)
	    fprintf(stderr, "Undef %s\n", p->name);
}
#endif

#ifdef DEBUG
static void
dprintf(a1, a2, a3, a4, a5, a6, a7, a8)
char *a1;
{
    if (debug)
	printf(a1, a2, a3, a4, a5, a6, a7, a8);
}
#endif

static void
relocate(fi, raddr, size, symtab, taddr, daddr)
FILE *fi;
coreaddr raddr;
int size;
struct nlist *symtab;
coreaddr taddr, daddr;
{
    int i;
    struct relocation_info rel;
    coreaddr d, adr;
    coreaddr p;
    char *name;
    int type;
#if MIPSCOFF
    int nreflo = 0;
    int reflo1 = 0;
#endif

#if MIPSCOFF
    raddr = taddr;
#endif
    for(i = 0; i < size; i+=sizeof(struct relocation_info)) {
	fread(&rel, sizeof(struct relocation_info), 1, fi);
	adr = RELOC_ADDRESS(&rel);
	p = (coreaddr)(raddr + adr);
#if MIPSCOFF
	if (rel.r_type == R_REFHI) {
	    coreaddr q;
	    struct relocation_info nextrel;

	    fread(&nextrel, sizeof(struct relocation_info), 1, fi);
	    if (nextrel.r_type != R_REFLO) {
		fprintf(stderr, "missing REFLO\n");
		exit(1);
	    }
	    fseek(fi, -sizeof(struct relocation_info), 1);
	    oldvalue = (*(long*)p & 0xffff) << 16;
	    q = (coreaddr)(raddr + RELOC_ADDRESS(&nextrel));
	    reflo1 = *(long*)q & 0xffff;
	    oldvalue |= reflo1;
	    if (oldvalue & 0x8000) oldvalue -= 0x10000;	/* compensate for sign extension in add */
	    nreflo = 0;
	} else if (rel.r_type == R_REFLO) {
	    nreflo++;
	    if (nreflo == 2) {
		/* we are trying to reuse a REFHI */
		oldvalue = oldvalue & 0xffff0000 | *(long*)p & 0xffff; /* update oldvalue to reflect the new REFLO */
	    } else if (nreflo > 2) {
		fprintf(stderr, "nreflo > 2\n");
		exit(1);
	    }
	} else {
	    oldvalue = 0;
	}
#endif
	if (RELOC_EXTERN_P(&rel)) {
	    name = symtab[RELOC_SYMBOL(&rel)].n_un.n_name;
#ifdef DEBUG
	    if (debug > 2)
		dprintf("reloc %4x, sym=%-14s (%3d), pc=%d, len=%d, bsr=%d, disp=%d\n",
			adr, name, RELOC_SYMBOL(&rel),
			RELOC_PCREL_P(&rel), RELOC_TARGET_SIZE(&rel), RELOC_MEMORY_SUB_P(&rel), RELOC_DISP(&rel));
#endif /*DEBUG*/
	    d = ilookup(name);
	    if (!d)
		addundef(name, (coreaddr)p, &rel, taddr);
	} else if (DO_PCREL || !RELOC_PCREL_P(&rel)) { /* don't relocate local pc-relative stuff (already done) */
	    type = RELOC_TYPE(&rel);
#ifdef DEBUG
	    if (debug > 2)
		dprintf("reloc %4x, %02x, pc=%d, len=%d, bsr=%d, disp=%d\n", adr, type,
			RELOC_PCREL_P(&rel), RELOC_TARGET_SIZE(&rel), RELOC_MEMORY_SUB_P(&rel), RELOC_DISP(&rel));
#endif /*DEBUG*/
	    switch(type) {
	    case N_TEXT:
	    case N_TEXT|N_EXT:
	    case N_DATA:
	    case N_DATA|N_EXT:
	    case N_BSS:
	    case N_BSS|N_EXT:
		break;
	    default:
		error("bad reloc type", "");
		return;
	    }
	    d = (coreaddr)(type & N_TEXT ? taddr : daddr);
	} else
	    d = 0;
	if (d)
	    setval(p, d, &rel, taddr);
    }
}

static void
readobj(name)
char *name;
{
    FILE *fi;
    int j, nsyms;
    EXEC mag_exp;
    char *strp;
    int size;
    char *taddr, *daddr, *baddr;
    struct nlist *symtab;
#if !MIPSCOFF
    int csize;
    int strsiz;
    off_t o;
    struct stat stb;
#else
    int nhdr;
#endif
    int rdoffs = 0;
    
    fi = fopen(name, "r");
    if (fi == NULL) {
	error(name, "cannot open");
	return;
    }
#if MIPSCOFF
#ifdef irix
#define MIPSMAGIC MIPSEBMAGIC
#else
#define MIPSMAGIC MIPSELMAGIC
#endif
    fread((char *)&filehdr, sizeof filehdr, 1, fi);
    nhdr = filehdr.f_nscns;
    if (nhdr != 2 && nhdr != 3 || filehdr.f_magic != MIPSMAGIC || (filehdr.f_flags & F_RELFLG)) {
	error("Unimplemented object file type","");
	fclose(fi);
	return;
    }
#endif
    fread((char *)&mag_exp, 1, sizeof(mag_exp), fi);
    if (N_BADMAG(mag_exp)) {
	error(name, "bad format");
	fclose(fi);
	return;
    }
    size = mag_exp.a_text + mag_exp.a_data + mag_exp.a_bss;
#if MIPSCOFF
    {
	HDRR symhdr;
	SYMR *lsyms;
	EXTR *esyms;
	int i;

	fread((char *)&thdr, sizeof thdr, 1, fi);
	if (strcmp(thdr.s_name, ".text") != 0) {
	    error("Expected .text",thdr.s_name);
	    fclose(fi);
	    return;
	}
	if (nhdr == 3) {
	    fread((char *)&rdhdr, sizeof rdhdr, 1, fi);
	    if (strcmp(rdhdr.s_name, ".rdata") != 0) {
		error("Expected .rdata",rdhdr.s_name);
		fclose(fi);
		return;
	    }
	}
	fread((char *)&dhdr, sizeof dhdr, 1, fi);
	if (strcmp(dhdr.s_name, ".data") != 0) {
	    error("Expected .data",dhdr.s_name);
	    fclose(fi);
	    return;
	}

	fseek(fi, filehdr.f_symptr, 0);
	if (fread((char *)&symhdr, sizeof symhdr, 1, fi) != 1) {
	    error("no symbol table", "");
	    fclose(fi);
	    return;
	}
	lsyms = (SYMR*)xmalloc(sizeof(SYMR) * symhdr.isymMax);
	esyms = (EXTR*)xmalloc(sizeof(EXTR) * symhdr.iextMax);
	strp = xmalloc(symhdr.issMax+symhdr.issExtMax);
	if (esyms == 0 || lsyms == 0 || strp == 0) {
	    error("no memory", "");
	    fclose(fi);
	    return;
	}
	fseek(fi, symhdr.cbSymOffset, 0);
	fread(lsyms, sizeof(SYMR), symhdr.isymMax, fi);
	fseek(fi, symhdr.cbExtOffset, 0);
	fread(esyms, sizeof(EXTR), symhdr.iextMax, fi);
	fseek(fi, symhdr.cbSsOffset, 0);
	fread(strp, 1, symhdr.issMax, fi);
	fseek(fi, symhdr.cbSsExtOffset, 0);
	fread(strp+symhdr.issMax, 1, symhdr.issExtMax, fi);
	nlocals = symhdr.isymMax;

	nsyms = symhdr.isymMax+symhdr.iextMax;
	symtab = (struct nlist *)xmalloc(sizeof(struct nlist) * nsyms);
	for(i = 0; i < symhdr.isymMax; i++) {
	    symtab[i].n_un.n_name = strp + lsyms[i].iss;
	    symtab[i].n_type = convert(lsyms[i].sc, lsyms[i].st);
	    symtab[i].n_value = lsyms[i].value;
	}
	for(i = 0; i < symhdr.iextMax; i++) {
	    symtab[symhdr.isymMax + i].n_un.n_name = strp + symhdr.issMax + esyms[i].asym.iss;
	    symtab[symhdr.isymMax + i].n_type = convert(esyms[i].asym.sc, esyms[i].asym.st) | N_EXT;
	    symtab[symhdr.isymMax + i].n_value = esyms[i].asym.value;
	}
	free(lsyms);
	free(esyms);
    }
#else
    fseek(fi, N_SYMOFF(mag_exp), 0);
    /* avoid __udivsi3 using gcc */
    nsyms = divi(mag_exp.a_syms, sizeof(struct nlist));
    if (nsyms == 0) {
	error(name, "no name list");
	fclose(fi);
	return;
    }
    stat(name, &stb);
    if (N_STROFF(mag_exp) + sizeof (off_t) > stb.st_size) {
	error(name, "old format .o (no string table) or truncated file");
	fclose(fi);
	return;
    }
    o = ftell(fi);
    fseek(fi, sizeof(struct nlist) * nsyms, 1);
    if (fread((char *)&strsiz, sizeof(strsiz), 1, fi) != 1) {
	error(name, "no string table (old format .o?)");
	fclose(fi);
	return;
    }
    strp = (char *)xmalloc(strsiz);
    if (strp == NULL) {
	error(name, "ran out of memory");
	fclose(fi);
	return;
    }
    if (fread(strp+sizeof(strsiz), strsiz-sizeof(strsiz), 1, fi) != 1) {
	error(name, "error reading string table");
	fclose(fi);
	return;
    }
    symtab = (struct nlist *)xmalloc(mag_exp.a_syms);
    fseek(fi, N_SYMOFF(mag_exp), 0);
    fread((char *)symtab, sizeof(struct nlist), nsyms, fi);
    for (j = 0; j < nsyms; j++) {
	symtab[j].n_un.n_name = symtab[j].n_un.n_strx + strp;
	if (symtab[j].n_type == N_EXT && symtab[j].n_value != 0) {
	    csize = symtab[j].n_value;
#ifdef DEBUG
	    if (debug)
		dprintf("Convert comm to bss for %-25s, size %5d\n", symtab[j].n_un.n_name, csize);
#endif
	    symtab[j].n_value = size;
	    symtab[j].n_type = N_BSS | N_EXT;
	    size += csize;
	}
    }
#endif
#ifdef DEBUG
/*    dprintf("magic = %8o\n", mag_exp.a_magic);*/
    dprintf("text  = %8d\n", mag_exp.a_text);
    dprintf("data  = %8d\n", mag_exp.a_data);
    dprintf("bss   = %8d\n", mag_exp.a_bss);
    dprintf("size  = %8d\n", size);
#endif /*DEBUG*/
    taddr = xmalloc(size);
    daddr = taddr + mag_exp.a_text;
    baddr = daddr + mag_exp.a_data;
    if (debug)
	fprintf(stderr, "load %s: text=%08x, data=%08x, bss=%08x\n", name, taddr, daddr, baddr);
#ifdef DEBUG
    dprintf("  # name                    type    value\n");
#endif /*DEBUG*/
    for(j = 0; j < nsyms; j++) {
	int t = symtab[j].n_type & N_TYPE;
#ifdef DEBUG
	dprintf("%3d %-25s %02x %8x\n", j, symtab[j].n_un.n_name,
	       symtab[j].n_type, symtab[j].n_value);
#endif /*DEBUG*/
	if ((symtab[j].n_type & N_EXT) && (t == N_TEXT || t == N_DATA)) {
	    addglob(symtab[j].n_un.n_name, 
		    symtab[j].n_value + (coreaddr)(symtab[j].n_type & N_TEXT ? taddr :
						   symtab[j].n_type & N_DATA ? daddr :
						   baddr));
	}
    }
    fseek(fi, TXT_OFF(mag_exp), 0);
    fread(taddr, 1, mag_exp.a_text, fi);
#if MIPSCOFF
    if (nhdr == 3) {
	fseek(fi, RDAT_OFF(mag_exp), 0);
	rdoffs = rdhdr.s_size;
	fread(daddr, 1, rdoffs, fi);
    }
#endif
    fseek(fi, DAT_OFF(mag_exp), 0);
    fread(daddr+rdoffs, 1, mag_exp.a_data-rdoffs, fi);

    fseek(fi, N_TROFF(mag_exp), 0);
#ifdef DEBUG
    dprintf("text relocation\n");
#endif
    relocate(fi, taddr, TRELSIZE(&mag_exp), symtab, taddr, daddr);
#if MIPSCOFF
    if (rdhdr.s_nreloc != 0) {
	error("relocateable items in .rdata", "");
	fclose(fi);
	return;
    }
#endif
    fseek(fi, N_DROFF(mag_exp), 0);
#ifdef DEBUG
    dprintf("data relocation\n");
#endif
    relocate(fi, daddr+rdoffs, DRELSIZE(&mag_exp), symtab, taddr, daddr);

    free(symtab);
    free(strp);
    fclose(fi);
#if defined(mips)
    {
	extern int cacheflush PROTO((void *, int, int));
	cacheflush(taddr, mag_exp.a_text, BCACHE);    /* flush instruction cache */
    }
#endif
}

static void
error(n, s)
char *n, *s;
{
    static char ebuf[1000];
    sprintf(ebuf, "load: %s %s", n, s);
    load_errmsg = ebuf;
}

void
loadmodule(s)
char *s;
{
    static int init = 0;

    load_errmsg = 0;
    if (!init) {
	inittab();
	init = 1;
    }
    if (*s)
	readobj(s);
}

void
chkundefs()
{
    int i;

    for(i = 0; i < NHASH; i++)
	if (undeftabs[i]) {
	    error(undeftabs[i]->name, "undefined");
	    return;
	}
}

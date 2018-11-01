#include "node.h"

#if defined(__ARM) || defined(SOLARIS) || defined(HPUX) || defined(__CYGWIN32__)
void
loadsymbols()
{
}

char *
lookup(a)
unsigned long a;
{
    return "lookup";
}
#else
#include <a.out.h>

#if !defined(mips) && !defined(_AIX)
static struct nlist *syms;
static int nsyms;

static int cmpsym(a, b)
struct nlist *a, *b;
{
    if ((unsigned long)a->n_value < b->n_value)
	return -1;
    else if ((unsigned long)a->n_value > b->n_value)
	return 1;
    else
	return 0;
}
#endif

void
loadsymbols()
{
#if !defined(mips) && !defined(_AIX) && !defined(__osf__)
    extern char *progname;
    struct exec e;
    int fd, len, i;
    char *strings;

    fd = open(progname, 0, 0);
    if (fd < 0) {
	fprintf(stderr, "Cannot open program\n");
	syms = 0;
    }
    read(fd, &e, sizeof e);
    lseek(fd, N_SYMOFF(e), 0);
    syms = (struct nlist *)xmalloc(e.a_syms);
    read(fd, syms, e.a_syms);
    nsyms = e.a_syms / sizeof(struct nlist);
    lseek(fd, N_STROFF(e), 0);
    read(fd, &len, sizeof len);
    strings = (char *)xmalloc(len);
    read(fd, strings, len);
    for(i = 0; i < nsyms; i++)
	syms[i].n_un.n_name = strings + syms[i].n_un.n_strx - sizeof len;
    qsort(syms, nsyms, sizeof(struct nlist), cmpsym);
/*
    for(i = 0; i < nsyms; i++)
	fprintf(stderr, "%s %x\n", syms[i].n_un.n_name, syms[i].n_value);
*/
#endif
}

char *
lookup(a)
unsigned long a;
{
    static char b[100];
#if !defined(mips) && !defined(_AIX) && !defined(__osf__)
    int i;

    if (!syms) {
	sprintf(b, "%x", a);
	return b;
    }
    for(i = 0; i < nsyms; i++)
	if (a < syms[i].n_value) {
	    int r = a - syms[i-1].n_value;
	    sprintf(b, r ? "%s+%x" : "%s", syms[i-1].n_un.n_name, r);
	    return b;
	}
#endif
    sprintf(b, "?%x", a);
    return b;
}

#endif /* __ARM */

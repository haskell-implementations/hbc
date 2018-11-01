#include "runtime.h"
#include "vars.h"

#if defined(__svr4__)
FILE *fdopen();
#endif

extern Node HIATON;

#define SETT(p, t) ((p)->tv_sec = (t)/1000, (p)->tv_usec = (t)%1000 * 1000)

#ifdef __ARM
#define SYSV
#define DIR char
#endif

#ifdef __386BSD__
#define _cnt _r
#endif

int
hread(f)
FILE *f;
{
#if defined(SYSV) || defined(linux) || defined(__CYGWIN32__)
    return 0;
#else
    fd_set imask;
    int n;
    long d;
    struct timeval tv;
    static int firsttime = 1;
    static struct timeval lasthiaton;

    if (f->_cnt > 0)		/* extremely unportable! */
	return getc(f) & 0xff;
    FD_ZERO(&imask);
    FD_SET(fileno(f), &imask);
    if (MergeHiaton) {
	if (firsttime) {
	    SETT(&tv, Ttime);
	    firsttime = 0;
	} else {
	    gettimeofday(&tv, (struct timezone *)0);
	    d = (tv.tv_sec - lasthiaton.tv_sec)*1000 + 
		(tv.tv_usec - lasthiaton.tv_usec)/1000;
	    d = Ttime - d;
	    if (d <= 0)
		d = 1;
	    SETT(&tv, d);
	}
    } else {
	SETT(&tv, Ttime);
    }
    if (debug) {
	fprintf(stderr, "hiaton select %d.%06d ", tv.tv_sec, tv.tv_usec);
    }
#ifdef HPUX
#define fd_set int
#endif
    n = select(fileno(f)+1, (fd_set *)&imask, (fd_set *)0, (fd_set *)0, &tv);
#ifdef HPUX
#undef fd_set
#endif
    if (MergeHiaton) {
	if (n == 0) {
	    /* timeout has expired, produce hiaton and remember time */
	    gettimeofday(&lasthiaton, (struct timezone *)0);
	}
    }
    if (debug) {
	fprintf(stderr, "got %s\n", n == 1 ? "char" : "hiaton");
    }
    if (n == 1) {
	return getc(f) & 0xff;
    } else {
	return CHAROF(&HIATON);
    }
#endif
}

int
readchar(f, offs)
Int offs;
FILE *f;
{
    int c;
    extern char *bufp, outbuf[];

    if (offs != -1)
	fseek(f, offs, 0);	/* move to seek point if there is an offset */
    if (bufp != outbuf) {
	flushlow();
    }
/*    if(Hflag && isatty(fileno(f)))*/
    if(Hflag)
	c = hread(f);
    else {
	c = getc(f);
	if (c != EOF)
	    c &= 0xff;
    }
    if (debug > 1) {
	fprintf(stderr, "readchar %c %02x\n", c, c);
    }
    return c;
}

struct files {
    struct files *next;
    FILE *file;
    DIR *dir;
    int ref;
} *allfiles = 0;

void
initfiles()
{
    addgcfile(stdin, (DIR*)0);
    addgcfile(stdout, (DIR*)0);
    addgcfile(stderr, (DIR*)0);
}

int
closefile(f)
FILE *f;
{
    struct files **ff, *fi;
    if (f == stdin || f == stdout || f == stderr)
        return 0;			/* don't close these, they may reappear */
    if (debug) fprintf(stderr, "close %lx\n", (UInt)f);
    for(ff = &allfiles; *ff; ff = &(*ff)->next)
	if ((*ff)->file == f) {
	    fi = *ff;
	    *ff = (*ff)->next;
	    free((char *)fi);
	    break;
	}
    (void)fclose(f);
    return 1;
}

void
cclosedir(f)
DIR *f;
{
    struct files **ff, *fi;
    if (debug) fprintf(stderr, "close directory %lx\n", (UInt)f);
    for(ff = &allfiles; *ff; ff = &(*ff)->next)
	if ((*ff)->dir == f) {
	    fi = *ff;
	    *ff = (*ff)->next;
	    free((char *)fi);
	    break;
	}
    (void)closedir(f);
}

void
tocstring(s, cp, n)
PTR s;
char *cp;
int n;
{
    --n;
    while(!ISNIL(s)){
	if (--n > 0)
	    *cp++ = CHAROF(HDOF(s));
	s = TLOF(s);
    }
    *cp = '\0';
}

/* add a file to those that can be garbage collected */
void
addgcfile(f, d)
FILE *f;
DIR *d;
{

    struct files *fi;
    for(fi = allfiles; fi; fi = fi->next)
	if (fi->file == f && fi->dir == d)
	    return;
    fi = (struct files *)xmalloc(sizeof(struct files));
    fi->file = f;
    fi->dir = d;
    fi->ref = 0;
    fi->next = allfiles;
    allfiles = fi;
    if (debug) fprintf(stderr, "addgcfile %lx %lx\n", (Int)f, (Int)d);
}

#ifdef __ARM
char *fixname( /* char *name */);
#endif

PTR
openfile(s)
PTR s;
{
    extern int errno;
    char buf[1024];
    FILE *f;
    PTR res;
    
    tocstring(s, buf, sizeof buf);
#ifdef __ARM
    (void)fixname(buf);
#endif
    if (debug) fprintf(stderr, "opening %s ", buf);
#ifdef __ARM
    {
#else
    if (strcmp(buf, "/dev/fd0") == 0) {
	/* Many systems do not support /dev/fdN, simulate /dev/fd0. */
	f = fdopen(dup(0), "r");
    } else {
#endif
	f = fopen(buf, "r");
    }
#ifdef __ARM
    if (f == NULL) {
#else
    if (f == NULL && errno == EMFILE) {
#endif
	/* GC may free up some descriptors, so gc and retry */
	dogc();
	if (debug) fprintf(stderr, "EMFILE forces GC ");
	f = fopen(buf, "r");
    }
    if (debug) fprintf(stderr, "%lx\n", (UInt)f);
    if(f == NULL){
	res = mkno(mkcstring((char *)SYSERROR(errno)));
    } else {
	extern Tag INPUT;
	addgcfile(f, (DIR*)0);
	res = mkyes(mknode2(&INPUT, (Int)f, (Int)-1));
    }
    return res;
}

static PTR
mkintlist(a, n)
long *a, n;
{
    if (n == 0)
	return mknil();
    else
	return mkcons(mkint(*a), mkintlist(a+1, n-1));
}

PTR
statfile(s)
PTR s;
{
#ifdef __ARM
    return mkno(mkcstring("Not implemented"));
#else
    extern int errno;
    char buf[1024];
    int r;
    PTR res;
    struct stat sb;
    
    tocstring(s, buf, sizeof buf);
    if (debug) fprintf(stderr, "stating %s\n", buf);
    r = stat(buf, &sb);
    if(r < 0){
	res = mkno(mkcstring((char *)SYSERROR(errno)));
    } else {
	long a[11];
	a[0] = sb.st_dev;   a[1] = sb.st_ino;   a[2] = sb.st_mode;
	a[3] = sb.st_nlink; a[4] = sb.st_uid;   a[5] = sb.st_gid;
	a[6] = sb.st_rdev;  a[7] = sb.st_size;  a[8] = sb.st_atime;
	a[9] = sb.st_mtime; a[10]= sb.st_ctime;
	res = mkyes(mkintlist(a, sizeof a/sizeof a[0]));
    }
    return res;
#endif /* __ARM */
}

void
filegcinit()
{
    setfileref(stdin);
    setfileref(stdout);
    setfileref(stderr);
}

/* Close all unreferenced files.
** The ref bits are set by gc.  Also reset ref bits in
** preparation for next gc.
*/
void
filegcfinish()
{
    struct files **ff;

    for(ff = &allfiles; *ff; ) {
	if ((*ff)->ref) {
	    /* referenced */
	    (*ff)->ref = 0;
	    ff = &(*ff)->next;
	} else {
	    /* unreferenced */
	    if (debug) fprintf(stderr, "file gc %lx\n", (UInt)(*ff)->file);
	    if ((*ff)->file) {
	        if (!closefile((*ff)->file)) {
		    /* we did not close it */
		    ff = &(*ff)->next;
		    if (debug) fprintf(stderr, "don't close\n");
		}
	    } else {
		cclosedir((*ff)->dir);
	    }
	    /* closefile/cclosedir will unlink the current files struct */
	}
    }
}

/* called from M-code to set the gc ref */
int
setfileref(f)
FILE *f;
{
    struct files *fi;

    if (f==0 || f==(FILE*)-1L)
	return 0;
    for(fi = allfiles; fi; fi = fi->next) {
	if (fi->file == f) {
	    return fi->ref++;
	}
    }
    fprintf(stderr, "unknown file in setfileref %lx\n", (Int)f);
    finish(1);
    return 0;
}

/* called from M-code to set the gc ref */
int
setdirref(f)
DIR *f;
{
    struct files *fi;

    for(fi = allfiles; fi; fi = fi->next)
	if (fi->dir == f) {
	    return fi->ref++;
	}
    fprintf(stderr, "unknown file in setdirref %lx\n", (Int)f);
    finish(1);
    return 0;
}

/******************* Directories ****************/

PTR
copendir(s)
PTR s;
{
    extern int errno;
    char buf[1024];
    DIR *f;
    PTR res;
    
    tocstring(s, buf, sizeof buf);
    if (debug) fprintf(stderr, "opening directory %s ", buf);
    if(hp > ehp)
	dogc();
    f = opendir(buf);
#ifndef __ARM
    if (f == NULL && errno == EMFILE) {
	/* GC may free up some descriptors, so gc and retry */
	dogc();
	if (debug) fprintf(stderr, "EMFILE forces GC ");
	f = opendir(buf);
    }
#endif
    if (debug) fprintf(stderr, "%lx\n", (UInt)f);
    if(f == NULL){
	res = mkno(mkcstring((char *)SYSERROR(errno)));
    } else {
	extern Tag INPUTD;
	addgcfile((FILE*)0,f);
	res = mkyes(mknode2(&INPUTD, (Int)f, 0));
    }
    return res;
}

PTR
creaddir(d)
DIR *d;
{
#ifdef __ARM
    return 0;
#else
#if defined(__osf__) || defined(SOLARIS) || defined(HPUX) || defined(_AIX) || defined(__CYGWIN32__)
#define direct dirent
#endif
    struct direct *de;
    
    de = readdir(d);
    if (!de)
	return 0;
    if (debug > 1) {
	fprintf(stderr, "creaddir reads %s\n", de->d_name);
    }
    return mkestring(de->d_name);
#endif /* __ARM */
}

#ifdef __ARM
#undef SYSV
#endif


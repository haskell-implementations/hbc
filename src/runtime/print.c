#include "runtime.h"
#include "vars.h"

#define LINEBUF 1
int linebuf = 1;

static FILE *ofiles[10];

extern Node TOFILEA, TOFILE, TOSTDOUT, TOSTDERR,
            CCFLUSH, HIATON, CCBREAK, CCOOKED, CHIATON, CUNBUFF, TONEWS,
            CMHIATON, CDELAY, CINTRON, CINTROFF;

FILE *curoutfile;
#define sf curoutfile

#define S_NORMAL 1
#define S_NORMAL_CLOSE 2
#define S_TOFILE 3
#define S_TOFILEA 4
#define S_HIATON 5
#define S_MHIATON 6
#define S_DELAY 7
static int state;

void
printtop(f)
FILE *f;
{
    FILE *oldsf;

    ofiles[1] = stdout;
    ofiles[2] = stderr;
    if (debug > 2) fprintf(stderr, "printtop %lx\n", (unsigned int)f);
    flushlow();
    oldsf = sf;
    sf = f;
    state = S_NORMAL;
    print(*ep++);
    sf = oldsf;
}

void
printi(i)
Int i;
{
    fprintf(sf, "%ld", i);
}

void
printd(p)
PTR p;
{
#if IntSize == 8
    union { Int i; double d; } u;
    u.i = p->node30.val0;
#else
    union { int i[2]; double d; } u;
    u.i[0] = p->node30.val0; u.i[1] = p->node30.val1;
#endif
    fprintf(sf, "%g", u.d);
}

#if defined(ultrix) || defined(irix) || defined(HPUX)
static void
usleep(d)
unsigned long d;
{
#ifndef __ARM
#ifdef SYSV
#else
    struct timeval tv;
    extern int select();

    tv.tv_sec = d / 1000000;
    tv.tv_usec = d % 1000000;
#ifdef HPUX
#define fd_set int
#endif
    select(0, (fd_set *)0, (fd_set *)0, (fd_set *)0, &tv);
#ifdef HPUX
#undef fd_set
#endif
#endif
#endif /* __ARM */
}
#endif

#define MAXNAME 1024

#ifdef __ARM
char *fixname(name)
char *name;
{
  static char buffer[MAXNAME];
  char *last,*prev,*tmp = buffer;
  strcpy(buffer,name);
  if((last = strrchr(buffer,'.')) != 0 && ((last[1] != 0 && last[2] == 0) || !strcmp(last,".mc"))) {
    while(tmp!=last) {
      prev = tmp;
      if(*prev == '.') prev++;
      tmp = strchr(prev,'.');
    }
    strncpy(name,buffer,prev-buffer);
    name[prev-buffer] = 0;
    if(last[1] == 'M') strcat(name,"mc");
    else strcat(name,last+1);
    strcat(name,".");
    strncat(name,prev,last-prev);
  }
  return name;
}
#else
#define fixname(n) (n)
#endif

#define NONE (-2)

void
printflsh(endp, lastc)
unsigned char *endp;
int lastc;
{
    static int oldstate;
    static char name[MAXNAME];
    static int ni;
    extern SIGTYPE sigintr();

    extern unsigned char outbuf[], *endbuf;
    unsigned char *cur;
    int c;

    for(cur = outbuf; cur < endp || lastc != NONE; cur++) {
	if (cur < endp) {
	    c = *cur;
	} else {
	    c = lastc;
	    lastc = NONE;
	}
#if 1
	if (debug > 2) {
	    fprintf(stderr, "print %c %02x\n", c, c);
	}
#endif
	if (state == S_NORMAL && c >= 0) putc(c, sf); else /* just a speedup */
	switch (state) {
	case S_TOFILE:
	case S_TOFILEA:
	    if (c == '\n') {
		name[ni] = 0;
		sf = fopen(fixname(name), state == S_TOFILEA ? "a" : "w");
		if (sf == NULL) {
		    fprintf(stderr, "Cannot write to %s\n", name);
		    finish(1);
		}
		state = S_NORMAL_CLOSE;
	    } else if (ni < MAXNAME-1) {
		name[ni++] = c;
	    }
	    break;
	case S_HIATON:
	case S_MHIATON:
	    if (c < 0)
		Hflag = 0;
	    else {
		Hflag = 1;
		Ttime = c;
		MergeHiaton = state == S_MHIATON;
	    }
	    if (debug) {
		fprintf(stderr, "Turning on hiatonic input %d %d\n", MergeHiaton, Ttime);
	    }
	    state = oldstate;
	    break;
	case S_DELAY:
	    usleep(c*1000);
	    state = oldstate;
	    break;
	case S_NORMAL:
	case S_NORMAL_CLOSE:
	    if (c < 0) {
		if (c == CHAROF(&CHIATON)) {
		    oldstate = state;
		    state = S_HIATON;
		} else if (c == CHAROF(&CMHIATON)) {
		    oldstate = state;
		    state = S_MHIATON;
		} else if (c == CHAROF(&CCBREAK)) {
		    set_tty(1);
		} else if (c == CHAROF(&CCOOKED)) {
		    set_tty(0);
		} else if (c == CHAROF(&CCFLUSH)) {
		    fflush(sf);
		} else if (c == CHAROF(&CUNBUFF)) {
		    endbuf = 0;	/* force low level not to buffer */
		    setbuf(sf, NULL);
		} else if (c == CHAROF(&CINTRON)) {
		    if (debug) fprintf(stderr, "SIGINT -> sigintr\n");
		    signal(SIGINT, sigintr);
		} else if (c == CHAROF(&CINTROFF)) {
		    if (debug) fprintf(stderr, "SIGINT -> SIG_IGN\n");
		    signal(SIGINT, SIG_IGN);
		} else if (c == CHAROF(&CDELAY)) {
		    oldstate = state;
		    state = S_DELAY;
		} else {
		    if (state == S_NORMAL_CLOSE) {
			(void)fclose(sf);
		    } else {
			(void)fflush(sf);
		    }
		    state = S_NORMAL;
		    if (c == CHAROF(&TOSTDOUT)) {
			sf = ofiles[1];
		    } else if (c == CHAROF(&TOSTDERR)) {
			sf = ofiles[2];
#if 0
		    } else if (c == CHAROF(&TONEWS)) {
			sf = grab_news();
#endif
		    } else if (c == CHAROF(&TOFILE)) {
			state = S_TOFILE;
			ni = 0;
		    } else if (c == CHAROF(&TOFILEA)) {
			state = S_TOFILEA;
			ni = 0;
		    } else {
			sf = 0;
		    }
		    if (sf == 0) {
			fprintf(stderr, "Output to unopened file.\n");
			finish(1);
		    }
		}
	    } else {
		putc(c, sf);
#if LINEBUF
		if (linebuf && c == '\n') { /* Line buffered output */
		    fflush(sf);	/* Is this to slow ? */
		}
#endif
#if 0
		if (aflag)
		    putc(' ', sf);
#endif
	    }
	    break;
	default:
	    fprintf(stderr, "Internal state error in print.\n");
	    finish(1);
	}
    }
}

void
printc(c)
Int c;
{
    printflsh((char *)0, c);
}

void
prints(s)
char *s;
{
    if (debug > 1) {
	fprintf(stderr, "prints %s\n", s);
    }
    fputs(s, sf);
#if 0
    if (aflag)
	putc(' ', sf);
#endif
#if LINEBUF
    if (linebuf && strchr(s, '\n')) {
	fflush(sf);
    }
#endif
}

void
printsn(s, n)
unsigned char *s;
Int n;
{
    if (debug > 1) {
	fprintf(stderr, "printsn %s\n", s);
    }
    if (n) {
	do
	    putc(*s++, sf);
	while (--n);
    }
#if 0
    if (aflag)
	putc(' ', sf);
#endif
#if LINEBUF
    if (linebuf)
	while(n--) {
	    if (*s++ == '\n') {
		fflush(sf);
		break;
	    }
	}
#endif
}

void
printbvek(p)
int *p;
{
    int i, j;
    fprintf(sf, "{BVEK:%d ", p[1]);
    for(i = 2; i < p[1]+2; i++){
	for(j = 0; j < 32; j++){
	    if(p[i] & (1 << j)){
		fprintf(sf, "1");
	    }else{
		fprintf(sf, "0");
	    }
	}
	fprintf(sf, " ");
    }
    fprintf(sf, "}");
}

/*ARGSUSED*/
void
printerr(p)
int **p;
{
    fprintf(stderr, "Illegal node to print %08x:%08x\n",p,*p);
    finish(1);
}

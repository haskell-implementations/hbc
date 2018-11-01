#include <stdio.h>
#include <stdlib.h>
#include <sys/ioctl.h>

#if defined(SYSV) || defined(SOLARIS) || defined(HPUX) || defined(IRIX) || defined (linux)
#include <sys/termio.h>
#else /* SYSV */
#if defined(__NetBSD__) || defined(__FreeBSD__)
#include <termios.h>
#define termio termios
#define TCGETA TIOCGETA
#define TCSETA TIOCSETA
#else /* NetBSD */
#include <sgtty.h>
#endif /* NetBSD */
#endif /* SYSV */

#define MAXHIST 100
#define MAXLINE 500

#define CTL(c) ((c) & 0x1f)
#define DEL 0x7f

static char *history[MAXHIST];
static int nhist = 0;

static char theline[MAXLINE];
static int linepos;

static FILE *inf;
static FILE *outf;

#define ISPRINT(c) ((c) < 0x20 || (c) >= 0x7f && (c) < 0xa0)

static int
lenofc(c)
int c;
{
    c &= 0xff;
    if (ISPRINT(c))
	return 2;
    else
	return 1;
}

static int
lenof(start, end)
int start, end;
{
    int l;
    for(l = 0; start < end; start++)
	l += lenofc(theline[start]);
    return l;
}

static void
backwards(l)
int l;
{
    while(--l >= 0)
	putc('\b', outf);
}

static void
backthis()
{
    backwards(lenofc(theline[linepos-1]));
}

static void
putfmtchar(c)
int c;
{
    c &= 0xff;
    if (ISPRINT(c)) {
	putc('^', outf);
	c ^= '@';
    }
    putc(c, outf);
}

static void
putline(start, end)
int start, end;
{
    int i, c;
    for(i = start; i < end; i++) {
	putfmtchar(theline[i]);
    }
}

static void
erasechar(p)
int p;
{
    int len = strlen(theline);
    int l = lenofc(theline[p]);
    int lenc = lenof(p, len);
    int i;

    for(i = p; i < len-1; i++)
	theline[i] = theline[i+1];
    theline[i] = 0;
    putline(p, len-1);
    while(--l >= 0)
	putc(' ', outf);
    backwards(lenc);
}

static void
erasechars(n)
int n;
{
    int i;
    for(i = 0; i < n; i++)
	putc(' ', outf);
    backwards(n);
}

static void
inschar(c)
int c;
{
    int len = strlen(theline) + 1;
    int i;
    if (len >= MAXLINE-1)
	return;
    for(i = len; i > linepos; i--)
	theline[i] = theline[i-1];
    theline[i] = c;
    putline(linepos++, len);
    backwards(lenof(linepos, len));
}

static void
startofline()
{
    backwards(lenof(0, linepos));
}

static void
endofline()
{
    putline(linepos, strlen(theline));
}

static void
eraseline()
{
    startofline();
    erasechars(lenof(0, strlen(theline)));
}

static void
addline(s)
char *s;
{
    if (nhist >= MAXHIST) {
	int i;
	free(history[0]);
	for(i = 1; i < MAXHIST; i++)
	    history[i-1] = history[i];
	nhist--;
    }
    history[nhist++] = s;
}

static int
readl()
{
    int c, hpos, h;

    hpos = nhist;
    putline(0, linepos);
    for(;;) {
	c = getc(inf);
	if (c == EOF) {
	    if (ferror(inf))
	        exit(1);
	    else
	        return 0;
	}
	if (c == '\033' && getc(inf) == '[') {
	    switch(getc(inf)) {
	    case 'C': c = CTL('f'); break;
	    case 'D': c = CTL('b'); break;
	    case 'A': c = CTL('p'); break;
	    case 'B': c = CTL('n'); break;
	    }
	}
	switch(c) {
	case '\n':
	case '\r':
	    putc('\n', outf);
	    return 1;
	case DEL:
	case CTL('h'):
	    if (linepos <= 0)
		break;
	    backthis();
	    erasechar(--linepos);
	    break;
	case CTL('d'):
	    if (strlen(theline) == 0)
		return 0;
	    if (linepos >= strlen(theline))
		break;
	    erasechar(linepos);
	    break;
	case CTL('f'):
	    if (linepos >= strlen(theline))
		break;
	    putfmtchar(theline[linepos++]);
	    break;
	case CTL('b'):
	    if (linepos <= 0)
		break;
	    backthis();
	    --linepos;
	    break;
	case CTL('a'):
	    startofline();
	    linepos = 0;
	    break;
	case CTL('e'):
	    endofline();
	    linepos = strlen(theline);
	    break;
	case CTL('k'):
	    erasechars(lenof(linepos, strlen(theline)));
	    theline[linepos] = 0;
	    break;
	case CTL('q'):
	    inschar(getc(inf));
	    break;
	case CTL('p'):
	    for(h = hpos-1; h >= 0; h--) {
		if (strncmp(history[h], theline, linepos) == 0
		    && strcmp(history[h], theline) != 0) {
		    hpos = h;
		    eraseline();
		    strcpy(theline, history[hpos]);
		    putline(0, strlen(theline));
		    backwards(lenof(linepos, strlen(theline)));
		    break;
		}
	    }
	    break;
	case CTL('n'):
	    for(h = hpos+1; h < nhist; h++) {
		if (strncmp(history[h], theline, linepos) == 0
		    && strcmp(history[h], theline) != 0) {
		    hpos = h;
		    eraseline();
		    strcpy(theline, history[hpos]);
		    putline(0, strlen(theline));
		    backwards(lenof(linepos, strlen(theline)));
		    break;
		}
	    }
	    break;
	case CTL('u'):
	    startofline();
	    linepos = 0;
	    erasechars(lenof(linepos, strlen(theline)));
	    theline[linepos] = 0;
	    break;
	case CTL('r'):
	    eraseline();
	    putline(0, strlen(theline));
	    backwards(lenof(linepos, strlen(theline)));
	    break;
	default:
	    if (c >= ' ')
		inschar(c);
	    break;
	}
    }
}

#if defined(SYSV) || defined(linux) || defined(SOLARIS) || defined(HPUX) || defined(__NetBSD__) || defined(IRIX) || defined(__FreeBSD__) || defined(__CYGWIN32__)
void
set_tty(set)
int set;
{
	static struct termio save, modes;
	static int sset = 0;
	if (set) {
	    if (!sset)
		ioctl(fileno(outf), TCGETA, &save);
	    modes = save;
	    modes.c_iflag &= ~(IXON | IXOFF | ISTRIP);
	    /*modes.c_oflag &= ~OPOST;*/
	    modes.c_lflag &= ~(ICANON | ECHO);
	    modes.c_cc[VMIN] = 1;
	    modes.c_cc[VTIME] = 0;
	    ioctl(fileno(outf), TCSETA, &modes);
	    sset = 1;
	} else if (sset) {
	    ioctl(fileno(outf), TCSETA, &save);
	}
}
#else
void
set_tty(set)
int set;
{
    static struct sgttyb save, modes;
    static int sset = 0;

    if (set) {
	if (!sset)
	    ioctl(fileno(outf), TIOCGETP, &save);
	modes = save;
	modes.sg_flags |= CBREAK;
	modes.sg_flags &= ~ECHO;
	ioctl(fileno(outf), TIOCSETP, &modes);
	sset = 1;
    } else if (sset) {
	ioctl(fileno(outf), TIOCSETP, &save);
    }
}
#endif

unsigned char *
readline(in, prompt, start)
FILE *in;
char *prompt, *start;
{
    char *s;

    inf = in;
    outf = stderr;
    set_tty(1);
    setbuf(outf, NULL);
    fputs(prompt, outf);
    strcpy(theline, start);
    linepos = strlen(theline);
    if (readl() == 0)
	return 0;
    s = malloc(strlen(theline) + 1);
    strcpy(s, theline);
    addline(s);
    return (unsigned char *)s;
}

int
getachar(interactive,f)
int interactive;
FILE *f;
{
#ifndef NOREADLINE
    if (interactive && isatty(fileno(f))) {
	static unsigned char *p = 0;

	if (p == 0) {
	    p = readline(f, "", "");
	    if (p == 0)
		return EOF;
	}
	if (*p == 0) {
	    p = 0;
	    return '\n';
	} else
	    return *p++;
    } else
#endif
	return getc(f);
}

#if 0
main() 
{
    char *l;

    for(;;) {
	l = readline("rd> ", "");
	printf("\n   '%s'\n", l);
    }
}
#endif

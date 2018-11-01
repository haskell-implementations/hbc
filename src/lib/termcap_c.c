#include <stdio.h>
#include "../runtime/node.h"
#include "../runtime/funs.h"

#ifdef PC
#undef PC
#endif

void tputs PROTO((char *, int, void (*)()));
int tgetent PROTO((char *, char *));
int tgetnum PROTO((char *));
char *tgoto PROTO((char *,int,int));

char PC;	/* padding character */
char *BC;
char *UP;
short ospeed;
static char *cl;	/* clear display */
static char *cm;	/* cursor movement */

static int li, co;	/* lines, columns */
static char *rawp;
static char rawbuffer[50];

static int init = 0;

static void GetTermCaps();

static void
putraw(c)
int c;
{
    *rawp++ = c;
}

PTR
MoveTo(colp, rowp)
PTR colp, rowp;
{
    int col = INTOF(colp), row = INTOF(rowp);
    GetTermCaps();
    rawp = rawbuffer;
    if (cm && *cm) {
	tputs(tgoto(cm, col, row), 1, putraw);
    }
    *rawp = 0;
    return mkestring(rawbuffer);
}

PTR
Columns()
{
    GetTermCaps();
    return mkint(co);
}

PTR
Lines()
{
    GetTermCaps();
    return mkint(li);
}

PTR
ClearScreen()			/* clear the whole screen and home */
{
    GetTermCaps();
    rawp = rawbuffer;
    if (cl && *cl) {
	tputs(cl, li, putraw);	/* send the clear screen code */
    }
    *rawp = 0;
    return mkestring(rawbuffer);
}

static char *
Mytgetstr(id, area)
register char *id, **area;
{
    register char *cp;
    char *tgetstr();

    if ((cp = tgetstr(id, area)) == (char *)0)
	return ("");
    return (cp);
}

static void
GetTermCaps()			/* read in the needed terminal capabilites */
{
    register int i;
    static char buffer[1024];	/* for string values */
    static char bp[1024];
    char *area = buffer;
    char *MyTerm;
    char *getenv();


    if (init)
	return;
    init = 1;
    MyTerm = getenv("TERM");
    if (!MyTerm[0])
	MyTerm = "dumb";
    i = tgetent(bp, MyTerm);
    if (i <= 0) {
	if (i == -1) {
	    fprintf(stderr, "Cannot open /etc/termcap.\n");
	    finish(1);
	} else if (i == 0) {
	    fprintf(stderr, "No entry for terminal type \"%s\"\n", getenv("TERM"));
	    finish(1);
	}
    }

    co = tgetnum("co");
    li = tgetnum("li");	

    UP = Mytgetstr("up", &area);
    BC = Mytgetstr("bc", &area);
    cl = Mytgetstr("cl", &area);
    cm = Mytgetstr("cm", &area);

    if (co < 2) co = 80;	/* just in case */
    if (li < 1) li = 24;
}

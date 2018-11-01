#include <stdio.h>
#include <string.h>
#include "Defines.h"
#include "Dimensions.h"
#include "Curves.h"
#include "HpFile.h"
#include "Axes.h"
#include "Key.h"
#include "Main.h"
#include "Marks.h"
#include "Utilities.h"

static void Prologue();
static void Variables();
static void BorderOutlineBox();
static void BigTitleOutlineBox();
static void TitleOutlineBox();
static void BigTitleText();
static void TitleText();

void PutPsFile()
{
    Prologue();
    Variables();
    BorderOutlineBox();

    if (bflag) {
	BigTitleOutlineBox();
        BigTitleText();
    } else {
	TitleOutlineBox();
	TitleText();
    }

    CurvesInit();

    Axes();
    Key();
    Curves();

    if (!yflag) Marks();

    fprintf(psfp, "showpage\n");
}


static void StandardSpecialComments();		/* forward */
static void EPSFSpecialComments();		/* forward */
static void Landscape();			/* forward */
static void Portrait();				/* forward */
static void Scaling();				/* forward */

static void Prologue()
{
float epsfscale;

    if (eflag) epsfscale = epsfwidth / (float) borderwidth;

    if (eflag) {
	EPSFSpecialComments(epsfscale);
    } else {
	StandardSpecialComments();
    }

    if (eflag) {
	Scaling(epsfscale);
    } else if (gflag) {
	Portrait();
    } else {
	Landscape();
    }
}

extern char *jobstring;
extern char *datestring;

static void StandardSpecialComments()
{
    fprintf(psfp, "%%!PS-Adobe-2.0\n");
    fprintf(psfp, "%%%%Title: %s\n", jobstring);
    fprintf(psfp, "%%%%Creator: %s (version %s)\n", programname, VERSION);
    fprintf(psfp, "%%%%CreationDate: %s\n", datestring);
    fprintf(psfp, "%%%%EndComments\n");
} 

static void EPSFSpecialComments(epsfscale)
float epsfscale;
{
    fprintf(psfp, "%%!PS-Adobe-2.0\n");
    fprintf(psfp, "%%%%Title: %s\n", jobstring);
    fprintf(psfp, "%%%%Creator: %s (version %s)\n", programname, VERSION);
    fprintf(psfp, "%%%%CreationDate: %s\n", datestring);
    fprintf(psfp, "%%%%BoundingBox: 0 0 %d %d\n", 
		(int) (borderwidth  * epsfscale + 0.5), 
	        (int) (borderheight * epsfscale + 0.5) );
    fprintf(psfp, "%%%%EndComments\n");
} 



static void Landscape()
{
    fprintf(psfp, "-90 rotate\n");
    fprintf(psfp, "%f %f translate\n", -(borderwidth + (float) START_Y), 
	          (float) START_X); 
}

static void Portrait()
{
    fprintf(psfp, "%f %f translate\n", (float) START_X, (float) START_Y); 
}

static void Scaling(epsfscale)
float epsfscale;
{
    fprintf(psfp, "%f %f scale\n", epsfscale, epsfscale);
}


static void Variables()
{
    fprintf(psfp, "/HE%d /Helvetica findfont %d scalefont def\n",
                  NORMAL_FONT, NORMAL_FONT);

    fprintf(psfp, "/HE%d /Helvetica findfont %d scalefont def\n", 
                  LARGE_FONT, LARGE_FONT);
}


static void BorderOutlineBox()
{
    fprintf(psfp, "newpath\n");
    fprintf(psfp, "0 0 moveto\n");
    fprintf(psfp, "0 %f rlineto\n", borderheight);
    fprintf(psfp, "%f 0 rlineto\n", borderwidth);
    fprintf(psfp, "0 %f rlineto\n", -borderheight);
    fprintf(psfp, "closepath\n");
    fprintf(psfp, "%f setlinewidth\n", borderthick);
    fprintf(psfp, "stroke\n");
}


static void BigTitleOutlineBox()
{
    fprintf(psfp, "newpath\n");
    fprintf(psfp, "%f %f moveto\n", borderspace,
                  borderheight - titleheight - borderspace);
    fprintf(psfp, "0 %f rlineto\n", titleheight);
    fprintf(psfp, "%f 0 rlineto\n", titlewidth);
    fprintf(psfp, "0 %f rlineto\n", -titleheight);
    fprintf(psfp, "closepath\n");
    fprintf(psfp, "%f setlinewidth\n", borderthick);
    fprintf(psfp, "stroke\n");

    fprintf(psfp, "%f %f moveto\n", borderspace,
                  borderheight - titleheight / 2 - borderspace);
    fprintf(psfp, "%f 0 rlineto\n", titlewidth);
    fprintf(psfp, "stroke\n");
}


static void TitleOutlineBox()
{
    fprintf(psfp, "newpath\n");
    fprintf(psfp, "%f %f moveto\n", borderspace, 
                  borderheight - titleheight - borderspace);
    fprintf(psfp, "0 %f rlineto\n", titleheight);
    fprintf(psfp, "%f 0 rlineto\n", titlewidth);
    fprintf(psfp, "0 %f rlineto\n", -titleheight);
    fprintf(psfp, "closepath\n");
    fprintf(psfp, "%f setlinewidth\n", borderthick);
    fprintf(psfp, "stroke\n");
}

static void EscapePrint();		/* forward */

static void BigTitleText()
{
float x, y;

    x = borderspace + titletextspace;
    y = borderheight - titleheight / 2 - borderspace + titletextspace;

    /* job identifier goes on top at the far left */

    fprintf(psfp, "HE%d setfont\n", TITLE_TEXT_FONT);
    fprintf(psfp, "%f %f moveto\n", x, y);
    fputc('(', psfp); 
    EscapePrint(jobstring, BIG_JOB_STRING_WIDTH);
    fprintf(psfp, ") show\n", jobstring);

    y = borderheight - titleheight - borderspace + titletextspace;

    /* area below curve gows at the botton, far left */

    fprintf(psfp, "HE%d setfont\n", TITLE_TEXT_FONT);
    fprintf(psfp, "%f %f moveto\n", x, y);
    fputc('(', psfp);
    CommaPrint(psfp, (int) areabelow);
    fprintf(psfp, " %s x %s)\n", valueunitstring, sampleunitstring); 
    fprintf(psfp, "show\n");

    /* date goes at far right */

    fprintf(psfp, "HE%d setfont\n", TITLE_TEXT_FONT);
    fprintf(psfp, "(%s)\n", datestring);
    fprintf(psfp, "dup stringwidth pop\n");
    fprintf(psfp, "%f\n", (titlewidth + borderspace) - titletextspace); 
    fprintf(psfp, "exch sub\n");
    fprintf(psfp, "%f moveto\n", y);
    fprintf(psfp, "show\n");
}


static void TitleText()
{
float x, y;
 
    x = borderspace + titletextspace;
    y = borderheight - titleheight - borderspace + titletextspace;
 
    /* job identifier goes at far left */
 
    fprintf(psfp, "HE%d setfont\n", TITLE_TEXT_FONT);
    fprintf(psfp, "%f %f moveto\n", x, y);
    fputc('(', psfp); 
    EscapePrint(jobstring, SMALL_JOB_STRING_WIDTH);
    fprintf(psfp, ") show\n", jobstring);
 
    /* area below curve is centered */
 
    fprintf(psfp, "HE%d setfont\n", TITLE_TEXT_FONT);
    fputc('(', psfp);
    CommaPrint(psfp, (int) areabelow);
    fprintf(psfp, " %s x %s)\n", valueunitstring, sampleunitstring);
 
    fprintf(psfp, "dup stringwidth pop\n");
    fprintf(psfp, "2 div\n");
    fprintf(psfp, "%f\n", titlewidth / 2);
    fprintf(psfp, "exch sub\n");
    fprintf(psfp, "%f moveto\n", y);
    fprintf(psfp, "show\n");
 
    /* date goes at far right */
 
    fprintf(psfp, "HE%d setfont\n", TITLE_TEXT_FONT);
    fprintf(psfp, "(%s)\n", datestring);
    fprintf(psfp, "dup stringwidth pop\n");
    fprintf(psfp, "%f\n", (titlewidth + borderspace) - titletextspace);
    fprintf(psfp, "exch sub\n");
    fprintf(psfp, "%f moveto\n", y);
    fprintf(psfp, "show\n");
}

/*
 *	Print a string s in width w, escaping characters where necessary.
 */

static void EscapePrint(s,w)
char* s; int w;
{
    for ( ; *s && w > 0; *s++, w--) {
	if (*s == '(') {		/* escape required */
	    fputc('\\', psfp);
	} else if (*s == ')') {
	    fputc('\\', psfp);
	}

	fputc(*s, psfp);
    }
}

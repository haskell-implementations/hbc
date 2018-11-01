#include <stdio.h>
#include "Curves.h"
#include "Dimensions.h"
#include "HpFile.h"
#include "Main.h"

static void Caret();

void Marks()
{
int i;
float m;

    for (i = 0; i < nmarks; i++) {
	m = (markmap[i] / xrange) * graphwidth;
	Caret(xpage(m), ypage(0.0), 4.0);
    }
}


/*
 * Draw a small white caret at (x,y) with width 2 * d
 */

static void Caret(x,y,d)
float x; float y; float d;
{
    fprintf(psfp, "%f %f moveto\n", x - d, y);
    fprintf(psfp, "%f %f rlineto\n",  d, -d);
    fprintf(psfp, "%f %f rlineto\n",  d,  d);
    fprintf(psfp, "closepath\n");

    fprintf(psfp, "gsave\n");
    fprintf(psfp, "1.0 setgray\n");
    fprintf(psfp, "fill\n");
    fprintf(psfp, "grestore\n");
    fprintf(psfp, "stroke\n");
}

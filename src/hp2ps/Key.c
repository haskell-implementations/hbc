#include <stdio.h>
#include <math.h>
#include "Defines.h"
#include "Dimensions.h"
#include "HpFile.h"
#include "Main.h"
#include "Shade.h"

static void KeyEntry();

void Key()
{
int i;
float c;
float dc;

    for (i = 0; i < nidents; i++)    /* count identifiers */ 
	;

    c  = graphy0;
    dc = graphheight / (float) (i + 1);

    for (i = 0; i < nidents; i++) {
	c += dc;
	KeyEntry(c, identtable[i]->name, ShadeOf(identtable[i]->name));
    }
}



static void KeyEntry(centreline, name, colour)
float centreline; char* name; float colour;
{
float namebase;
float keyboxbase;
float kstart;


    namebase = centreline - (float) (NORMAL_FONT / 2);
    keyboxbase = centreline - ((float) KEY_BOX_WIDTH / 2.0);

    kstart = graphx0 + graphwidth;

    fprintf(psfp, "%f %f moveto\n", kstart + borderspace, keyboxbase);
    fprintf(psfp, "0 %d rlineto\n", KEY_BOX_WIDTH);
    fprintf(psfp, "%d 0 rlineto\n", KEY_BOX_WIDTH);
    fprintf(psfp, "0 %d rlineto\n", -KEY_BOX_WIDTH);
    fprintf(psfp, "closepath\n");

    fprintf(psfp, "gsave\n"); 
    /* old: fprintf(psfp, "%f setgray\n", colour); */
    SetGray(psfp, colour);
    fprintf(psfp, "fill\n");
    fprintf(psfp, "grestore\n");
    fprintf(psfp, "stroke\n");

    fprintf(psfp, "HE%d setfont\n", NORMAL_FONT);
    fprintf(psfp, "%f %f moveto\n", kstart + (float) KEY_BOX_WIDTH + 2 * borderspace, namebase);

    fprintf(psfp, "(%s) show\n", name); 
}

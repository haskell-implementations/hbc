#include <stdio.h>
#include <string.h>
#include "Curves.h"
#include "Defines.h"
#include "Dimensions.h"
#include "HpFile.h"
#include "Main.h"
#include "Utilities.h"

static void XAxis();
static void YAxis();

static void XAxisMark();
static void YAxisMark();

static float Round();

void Axes()
{
    XAxis();
    YAxis();
}


static void XAxisMark(x, num)
float x; float num;
{
    /* calibration mark */
    fprintf(psfp, "%f %f moveto\n", xpage(x), ypage(0.0));
    fprintf(psfp, "0 -4 rlineto\n");
    fprintf(psfp, "stroke\n");

    /* number */
    fprintf(psfp, "HE%d setfont\n", NORMAL_FONT);
    fprintf(psfp, "(%.1f)\n", num);
    fprintf(psfp, "dup stringwidth pop\n");
    fprintf(psfp, "2 div\n");
    fprintf(psfp, "%f exch sub\n", xpage(x));
    fprintf(psfp, "%f moveto\n", borderspace);
    fprintf(psfp, "show\n");
}





#define N_X_MARKS   	 7
#define XFUDGE   	15	

extern float xrange;
extern char *sampleunitstring;

static void XAxis()
{
float increment, i; 
float t, x;
float legendlen;
 
    /* draw the x axis line */
    fprintf(psfp, "%f %f moveto\n", xpage(0.0), ypage(0.0));
    fprintf(psfp, "%f 0 rlineto\n", graphwidth);
    fprintf(psfp, "%f setlinewidth\n", borderthick);
    fprintf(psfp, "stroke\n"); 

    /* draw x axis legend */
    fprintf(psfp, "HE%d setfont\n", NORMAL_FONT);
    fprintf(psfp, "(%s)\n", sampleunitstring);
    fprintf(psfp, "dup stringwidth pop\n");
    fprintf(psfp, "%f\n", xpage(0.0) + graphwidth);
    fprintf(psfp, "exch sub\n");
    fprintf(psfp, "%f moveto\n", borderspace);
    fprintf(psfp, "show\n");


    /* draw x axis scaling */

    increment = Round(xrange / (float) N_X_MARKS);

    t = graphwidth / xrange;
    legendlen = StringSize(sampleunitstring) + (float) XFUDGE;
 
    for (i = samplemap[0]; i < samplemap[nsamples - 1]; i += increment) {
        x = (i - samplemap[0]) * t;  
 
        if (x < (graphwidth - legendlen)) { 
            XAxisMark(x,i);
        } 
    } 
}

typedef enum {MEGABYTE, KILOBYTE, BYTE} mkb; 

static void YAxisMark(y, num, unit)
float y; float num; mkb unit;
{
    /* calibration mark */
    fprintf(psfp, "%f %f moveto\n", xpage(0.0), ypage(y));
    fprintf(psfp, "-4 0 rlineto\n");
    fprintf(psfp, "stroke\n");
 
    /* number */
    fprintf(psfp, "HE%d setfont\n", NORMAL_FONT);

    switch (unit) {
    case MEGABYTE :
	fprintf(psfp, "(");
	CommaPrint(psfp, (int) (num / 1e6 + 0.5));
	fprintf(psfp, "M)\n");
	break;
    case KILOBYTE :
	fprintf(psfp, "(");
	CommaPrint(psfp, (int) (num / 1e3 + 0.5));
	fprintf(psfp, "k)\n");
	break;
    case BYTE:
	fprintf(psfp, "(");
	CommaPrint(psfp, (int) (num + 0.5));
	fprintf(psfp, ")\n");
	break;
    }

    fprintf(psfp, "dup stringwidth\n");
    fprintf(psfp, "2 div\n");
    fprintf(psfp, "%f exch sub\n", ypage(y));

    fprintf(psfp, "exch\n");
    fprintf(psfp, "%f exch sub\n", graphx0 - borderspace);

    fprintf(psfp, "exch\n");
    fprintf(psfp, "moveto\n");
    fprintf(psfp, "show\n");
}


#define N_Y_MARKS 	 7	
#define YFUDGE 		15 

extern float yrange;
extern char *valueunitstring;

static void YAxis()
{
float increment, i;
float t, y;
float legendlen;
mkb unit;

    /* draw the y axis line */
    fprintf(psfp, "%f %f moveto\n", xpage(0.0), ypage(0.0));
    fprintf(psfp, "0 %f rlineto\n", graphheight);
    fprintf(psfp, "%f setlinewidth\n", borderthick);
    fprintf(psfp, "stroke\n");

    /* draw y axis legend */
    fprintf(psfp, "gsave\n");
    fprintf(psfp, "HE%d setfont\n", NORMAL_FONT);
    fprintf(psfp, "(%s)\n", valueunitstring);
    fprintf(psfp, "dup stringwidth pop\n");
    fprintf(psfp, "%f\n", ypage(0.0) + graphheight);
    fprintf(psfp, "exch sub\n");
    fprintf(psfp, "%f exch\n", xpage(0.0) - borderspace);
    fprintf(psfp, "translate\n");
    fprintf(psfp, "90 rotate\n");
    fprintf(psfp, "0 0 moveto\n");
    fprintf(psfp, "show\n");
    fprintf(psfp, "grestore\n");

    /* draw y axis scaling */
    increment = max( yrange / (float) N_Y_MARKS, 1.0);
    increment = Round(increment);

    if (increment >= 1e6) {
	unit = MEGABYTE;
    } else if (increment >= 1e3) {
	unit = KILOBYTE;
    } else {
	unit = BYTE;
    }	

    t = graphheight / yrange; 
    legendlen = StringSize(valueunitstring) + (float) YFUDGE; 

    for (i = 0.0; i <= yrange; i += increment) {
        y = i * t;

        if (y < (graphheight - legendlen)) {
            YAxisMark(y, i, unit);
        }
    } 
}


/*
 *      Find a "nice round" value to use on the axis.
 */



static float Round(y)
float y;
{
int i;

    for (i = 0; y > 10.0e-3; y /= 10.0, i++)
        ;

    if (y > 4.0e-3) {
        y = 5.0e-3;
    } else if (y > 1.0e-3) {
        y = 2.0e-3;
    } else {
        y = 1.0e-3;
    }   

    for ( ; i > 0; y = y * 10.0, i--)
        ;

    return(y);
}

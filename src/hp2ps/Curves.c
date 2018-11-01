#include <stdio.h>
#include <math.h>
#include "Defines.h"
#include "Dimensions.h"
#include "HpFile.h"
#include "Main.h"
#include "Shade.h"
#include "Utilities.h"

static float *x;		/* x and y values  */
static float *y;
static float yofs;              /* layer offset (used if lflag) */

static float *py;		/* previous y values */

static void Curve();				/* forward */
static void ShadeCurve();			/* forward */


void Curves()
{
int i;
    
    for (i = 0; i < nidents; i++) {
        Curve(identtable[i]);
    }
}

/*
 *      Draw a curve, and fill the area that is below it and above 
 *	the previous curve.
 */

static void Curve(e)
struct entry* e;
{
struct chunk* ch;
int j;
  
    if (lflag) {
        for (ch = e->chk; ch; ch = ch->next) {
            for (j = 0; j < ch->nd; j++) {
	        y[ ch->d[j].bucket ] = ch->d[j].value;
	    }
	}
    } else {
        for (ch = e->chk; ch; ch = ch->next) {
            for (j = 0; j < ch->nd; j++) {
	        y[ ch->d[j].bucket ] += ch->d[j].value;
   	    }
        }    
    }

    ShadeCurve(x, y, py, ShadeOf(e->name));
}


static void PlotCurveLeftToRight();		/* forward */
static void PlotCurveRightToLeft();		/* forward */
static float LayerPlotCurveRightToLeft();	/* forward */

static void SaveCurve();			/* forward */

/*
 *	Map virtual x coord to physical x coord 
 */
 
float xpage(x)
float x;
{
    return (x + graphx0); 
}



/*
 *	Map virtual y coord to physical y coord 
 */
 

float ypage(y)
float y;
{
    return (y + graphy0); 
}


/*
 *	Fill the region bounded by two splines, using the given 
 *	shade.
 */

static void ShadeCurve(x, y, py, shade)
float *x; float *y; float *py; float shade;
{
    if (lflag) {
        fprintf(psfp, "%f %f moveto\n", xpage(x[0]), ypage(yofs));
        fprintf(psfp, "%f %f lineto\n", xpage(x[nsamples-1]), ypage(yofs));
        yofs += LayerPlotCurveRightToLeft(x, y);
    } else {
        fprintf(psfp, "%f %f moveto\n", xpage(x[0]), ypage(py[0]));
        PlotCurveLeftToRight(x, py);

        fprintf(psfp, "%f %f lineto\n", xpage(x[nsamples - 1]), 
                                        ypage(y[nsamples - 1]));
        PlotCurveRightToLeft(x, y);
   }


    fprintf(psfp, "closepath\n");

    fprintf(psfp, "gsave\n");

    /* old: fprintf(psfp, "%f setgray\n", shade); */
    SetGray(psfp, shade);
    fprintf(psfp, "fill\n");

    fprintf(psfp, "grestore\n");
    fprintf(psfp, "stroke\n");

    if (!lflag) SaveCurve(y, py);
}

static void PlotCurveLeftToRight(x,y)
float *x; float *y;
{
int i;

    for (i = 0; i < nsamples; i++) {
        fprintf(psfp, "%f %f lineto\n", xpage(x[i]), ypage(y[i]));
    }
}


static void PlotCurveRightToLeft(x,y)
float *x; float *y;
{
int i;

    for (i = nsamples - 1; i >= 0; i-- ) {
        fprintf(psfp, "%f %f lineto\n", xpage(x[i]), ypage(y[i]));
    }
}

static float LayerPlotCurveRightToLeft(x,y)
float *x; float *y;
{
int i;
float m = 0.0;

    fprintf(psfp, "%f %f lineto\n", xpage(x[nsamples-1]), ypage(yofs));
    for (i = nsamples - 2; i > 0; i-- ) {
        fprintf(psfp, "%f %f lineto\n", xpage(x[i]), ypage(y[i]+yofs));
	if (y[i] > m) m = y[i];
    }
    fprintf(psfp, "%f %f lineto\n", xpage(x[0]), ypage(yofs));

    return m;
}


/*
 *	Save the curve coordinates stored in y[] in py[].
 */

static void SaveCurve(y, py)
float *y; float* py;
{
int i;

    for (i = 0; i < nsamples; i++) {
	py[i] = y[i];
    }
}

extern float xrange;

void CurvesInit()
{
int i;

    x  =  (float*) xmalloc(nsamples * sizeof(float));
    y  =  (float*) xmalloc(nsamples * sizeof(float));
    py =  (float*) xmalloc(nsamples * sizeof(float));
    yofs = 0.0;

    for (i = 0; i < nsamples; i++) {
        x[i] = ((samplemap[i] - samplemap[0])/ xrange) * graphwidth;
        y[i] = py[i] = 0.0; 
    }
}

#include <stdio.h>
#include <string.h>
#include "Defines.h"
#include "AuxFile.h"
#include "AreaBelow.h"
#include "Dimensions.h"
#include "HpFile.h"
#include "PsFile.h"
#include "Reorder.h"
#include "Scale.h"
#include "TopTwenty.h"
#include "TraceElement.h"
#include "Deviation.h"
#include "Error.h"
#include "Utilities.h"

int pflag = 0;		/* read auxiliary file			*/
int eflag = 0;		/* scaled EPSF 				*/ 
int dflag = 0;		/* sort by standard deviation		*/
int gflag = 0;		/* output suitable for previewer	*/
int yflag = 0; 		/* ignore marks				*/
int bflag = 0; 		/* use a big title box			*/
int sflag = 0;		/* use a small title box		*/
int lflag = 0;          /* output a layered image               */
int Cflag = 0;		/* produce colour instead of gray scale */

int filter;		/* true when running as a filter	*/

extern double atof();

static float WidthInPoints();		/* forward */
static FILE* Fp();			/* forward */

char *hpfile;
char *psfile;
char *auxfile;

char *programname;

static char *pathnamestr;
static char *basenamestr;

FILE* hpfp;
FILE* psfp;
FILE* auxfp;

float xrange = 0.0;
float yrange = 0.0;

float auxxrange = 0.0;
float auxyrange = 0.0;

float epsfwidth;
float areabelow;

int nsamples;
int nmarks;
int nidents;

int main(argc, argv)
int argc;
char* argv[];
{

    programname = copystring(Basename(argv[0]));

    argc--, argv++;
    while (argc && argv[0][0] == '-') {
        while (*++*argv)
            switch(**argv) {
	    case 'p':
                pflag++;
                break;
	    case 'e':
		eflag++;
                epsfwidth = WidthInPoints(*argv + 1);
                goto nextarg;
	    case 'd':
		dflag++;
                goto nextarg;
	    case 'g':
		gflag++;
		goto nextarg;
	    case 'y':
		yflag++;
		goto nextarg;
	    case 'b':
		bflag++;
		goto nextarg;
	    case 's':
		sflag++;
		goto nextarg;
	    case 'l':
		lflag++;
		goto nextarg;
	    case 'C':
		Cflag++;
		goto nextarg;
	    case '?':
	    default:
		Usage();
            }
nextarg: ;
        argc--, argv++;
    }

    hpfile = "stdin";
    psfile = "stdout";

    hpfp = stdin;
    psfp = stdout;

    filter = argc < 1;



    if (!filter) {
	pathnamestr = copystring(argv[0]);
	DropSuffix(pathnamestr, ".hp");
	basenamestr = copystring(Basename(pathnamestr));

        hpfp  = Fp(pathnamestr, &hpfile, ".hp", "r"); 
	psfp  = Fp(basenamestr, &psfile, ".ps", "w"); 

	if (pflag) auxfp = Fp(basenamestr, &auxfile, ".aux", "r");
    }


    GetHpFile(hpfp);

    if (!filter && pflag) GetAuxFile(auxfp);


    TraceElement();

    if (dflag) Deviation();

    if (pflag) Reorder();

    TopTwenty();

    Dimensions();

    areabelow = AreaBelow();

    Scale();

    PutPsFile();

    if (!filter) {
        auxfp = Fp(basenamestr, &auxfile, ".aux", "w");
	PutAuxFile(auxfp);
    } 

    return(0);
}



typedef enum {POINTS, INCHES, MILLIMETRES} pim;

static pim Units();		/* forward */

static float WidthInPoints(wstr)
char* wstr;
{
float result;

    result = atof(wstr);

    switch (Units(wstr)) {
	case INCHES:  		
	    result *= 72.0;
	    break;
        case MILLIMETRES:	
	    result *= 2.834646;
	    break;
        case POINTS:
	default: ;
    }

    return result;
}

	
static pim Units(wstr)
char* wstr;
{
int i;

    i = strlen(wstr) - 2;

    if (wstr[i] == 'p' && wstr[i+1] == 't') {
	return POINTS;
    } else if (wstr[i] == 'i' && wstr[i+1] == 'n') {
	return INCHES;	
    } else if (wstr[i] == 'm' && wstr[i+1] == 'm') {
	return MILLIMETRES;
    } else {
        return POINTS;
    }
}



static FILE* Fp(rootname, filename, suffix, mode)
char* rootname; char** filename; char* suffix; char* mode;
{
    *filename = copystring2(rootname, suffix);

    return(OpenFile(*filename, mode));
}

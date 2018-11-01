#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "Defines.h"
#include "Error.h"
#include "Utilities.h"
#include "Main.h"	/* Cflag */


#define SHADEMAP	(TWENTY * 2)  /* worst case, all key elements are new */

static struct {
	char* ident;
	float shade;
} shademap[ SHADEMAP ];

static int shademapindex = 0;

/*
 *	Set the shade to be used for "ident" to "shade".
 */

void ShadeFor(ident, shade)
char* ident; 
float shade;
{
    if (shademapindex < SHADEMAP) {
	shademap[ shademapindex ].ident = copystring(ident);
	shademap[ shademapindex ].shade = shade;
	shademapindex++;
    } else {
	Disaster("shade map overflow");
    }
}

/*
 *	Get the shade to be used for "ident" if there is one. 
 *	Otherwise, think of a new one.
 */

float ThinkOfAShade(); 		/* forward */

float ShadeOf(ident)
char* ident;
{
int i;
float shade;

    for (i = 0; i < shademapindex; i++) {
	if (strcmp(shademap[i].ident, ident) == 0) {	/* got it */
	    return(shademap[i].shade);
	}
    }

    shade = ThinkOfAShade();

    ShadeFor(ident, shade);

    return shade; 
}



#define N_SHADES 10 

static float shades[ N_SHADES ] = {
    0.00000, 0.20000, 0.60000, 0.30000, 0.90000, 
    0.40000, 1.00000, 0.70000, 0.50000,  0.80000
};

float ThinkOfAShade()
{
static int thisshade = 0;

float x;

    x = shades[ thisshade ]; 
    thisshade = (thisshade + 1) % N_SHADES;
    return x; 
}

void SetGray(fp, shade)
FILE *fp;
float shade;
{
    if (Cflag) {
	float r = shade < 0.25 ? 4.0 * shade :
		  shade < 0.75 ? 2.0 * (0.75 - shade) :
				 4.0 * (shade - 0.75);
	/* float g = shade < 0.5 ? 0 : 2.0 * (shade - 0.5); */
	float g = shade < 0.5 ? 0 : shade;
	/* float g = shade < 0.6 ? 0 : 1.0; */
	float b = 1.0 - r; /* shade > 0.5 ? 0 : 2.0 * (0.5 - shade); */
        fprintf(fp, "%f %f %f setrgbcolor\n", r, g, b);
    }
    else {
        fprintf(fp, "%f setgray\n", shade);
    }
}

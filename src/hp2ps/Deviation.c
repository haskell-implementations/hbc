#include <stdio.h>
#include <math.h>
#include "Defines.h"
#include "Main.h"
#include "Error.h"
#include "HpFile.h"
#include "Utilities.h"

extern void free();

/*
 *	Reorder the identifiers in the identifier table so that the
 *	ones whose data points exhibit the mininal standard deviation
 *	come first.	
 */

extern int nsamples;

void Deviation()
{
    int i;
    int j;
    float dev;
    struct chunk* ch;
    int min;
    float t;
    struct entry* e;
    float *averages; 
    float *deviations;

    averages   = (float*) xmalloc(nidents * sizeof(float));
    deviations = (float*) xmalloc(nidents * sizeof(float));

    /* find averages */

    for (i = 0; i < nidents; i++) {
	averages[ i ] = 0.0;
    }
 
    for (i = 0; i < nidents; i++) {
        for (ch = identtable[i]->chk; ch; ch = ch->next) {
	    for (j = 0; j < ch->nd; j++) {
	        averages[ i ] += ch->d[j].value; 
	    }
        }
    }    

    for (i = 0; i < nidents; i++) {
        averages[ i ] /= (float) nsamples;
    }

    /* calculate standard deviation */

    for (i = 0; i < nidents; i++) {
	deviations[ i ] = 0.0;
    }
 
    for (i = 0; i < nidents; i++) {
        for (ch = identtable[i]->chk; ch; ch = ch->next) {
	    for (j = 0; j < ch->nd; j++) {
		dev = ch->d[j].value - averages[i];
	        deviations[ i ] += dev * dev; 
	    }
        }
    }

    for (i = 0; i < nidents; i++) {
        deviations[ i ] = (float) sqrt ((double) (deviations[ i ] / 
		                        (float) (nsamples - 1)));
    }


    /* sort on basis of standard deviation */

    for (i = 0; i < nidents-1; i++) {
	min = i; 
	for (j = i+1; j < nidents; j++) {
	    if (deviations[ j ] < deviations[ min ]) {
		min = j;
	    }
	}

        t = deviations[ min ]; 
	deviations[ min ] = deviations[ i ];	
	deviations[ i ] = t;

        e = identtable[ min ];
	identtable[ min ] = identtable[ i ];
	identtable[ i ] = e;
    } 	

    free(averages);
    free(deviations);
}

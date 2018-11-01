#include <stdio.h>
#include "Defines.h"
#include "Error.h"
#include "HpFile.h"
#include "Main.h"
#include "Utilities.h"

extern void free();

/*
 *      Return the area enclosed by all of the curves. The algorithm
 *	used is the same as the trapizoidal rule for integration. 
 */
 
float AreaBelow()
{
    int i;
    int j;
    int bucket;
    float value;
    struct chunk* ch;
    float area;
    float trap;
    float base;
    float *maxima;

    maxima = (float*) xmalloc(nsamples * sizeof(float));
    for (i = 0; i < nsamples; i++) {
        maxima[ i ] = 0.0;
    }   

    for (i = 0; i < nidents; i++) {
        for (ch = identtable[i]->chk; ch; ch = ch->next) {
            for (j = 0; j < ch->nd; j++) {
                bucket = ch->d[j].bucket;
                value  = ch->d[j].value;
		if (bucket >= nsamples)
		    Disaster("bucket out of range");
                maxima[ bucket ] += value;
            }   
        }    
    }    

    area = 0.0;

    for (i = 1; i < nsamples; i++) {
	base = samplemap[i] - samplemap[i-1];
        if (maxima[i] > maxima[i-1]) {
	    trap = base * maxima[i-1] + ((base * (maxima[i] - maxima[i-1]))/ 2);
	} else {
	    trap = base * maxima[i]   + ((base * (maxima[i-1] - maxima[i]))/ 2);
        }

	area += trap;
    }

    free(maxima);
    return area;
}

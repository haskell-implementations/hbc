#include <stdio.h>
#include "Defines.h"
#include "Main.h"
#include "HpFile.h"
#include "Error.h"
#include "Utilities.h"

/*
 *	Compute the total volume for each identifier, and the grand 
 *	total of these totals. The identifiers whose totals when 
 *	added together amount to less that a threshold percentage 
 *      (default 1%) of the grand total are considered to be ``trace
 *	elements'' and they are thrown away.	
 */

extern void free();

extern float thresholdpercent;

void TraceElement()
{
int i;
int j;
struct chunk* ch;
int grandtotal;
int min;
float t;
float p;
struct entry* e;
int *totals; 


    totals = (int*) xmalloc(nidents * sizeof(int));

    /* find totals */

    for (i = 0; i < nidents; i++) {
	totals[ i ] = 0;
    }
 
    for (i = 0; i < nidents; i++) {
        for (ch = identtable[i]->chk; ch; ch = ch->next) {
	    for (j = 0; j < ch->nd; j++) {
	        totals[ i ] += ch->d[j].value; 
	    }
        }
    }    

    /* sort on the basis of total */

    for (i = 0; i < nidents-1; i++) {
        min = i;
        for (j = i+1; j < nidents; j++) {
            if (totals[ j ] < totals[ min ]) {
                min = j;
            }
        }    

        t = totals[ min ];
        totals[ min ] = totals[ i ];
        totals[ i ] = t;

        e = identtable[ min ];
        identtable[ min ] = identtable[ i ];
        identtable[ i ] = e;
    }


    /* find the grand total */

    grandtotal = 0;

    for (i = 0; i < nidents; i++) {
        grandtotal += totals[ i ];
    }

    t = 0.0;	/* cumulative percentage */
   
    for (i = 0; i < nidents; i++) {
        p = (100.0 * (float) totals[i]) / (float) grandtotal;
	t = t + p; 
	if (t >= THRESHOLD_PERCENT) {
	    break;
	}
    }

    /* identifiers from 0 to i-1 should be removed */
    for (j = 0; i < nidents; i++, j++) {
	identtable[j] = identtable[i]; 
    }

    nidents = j;
    free(totals);
}

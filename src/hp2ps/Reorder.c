#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Defines.h"
#include "Error.h"
#include "HpFile.h"
#include "Utilities.h"


#define ORDERMAP	(TWENTY * 2)  /* worst case, all key elements are new */

static struct {
    char* ident;
    int order;
} ordermap[ ORDERMAP ];

static int ordermapindex = 0;


void OrderFor(ident, order)
char* ident; 
int order;
{
    if (ordermapindex < ORDERMAP) {
	ordermap[ ordermapindex ].ident = copystring(ident);
	ordermap[ ordermapindex ].order = order;
	ordermapindex++;
    } else {
	Disaster("order map overflow");
    }
}

/*
 *	Get the order of to be used for "ident" if there is one. 
 *	Otherwise, return 0 which is the minimum ordering value. 
 */

int OrderOf(ident)
char* ident;
{
int i;

    for (i = 0; i < ordermapindex; i++) {
	if (strcmp(ordermap[i].ident, ident) == 0) {	/* got it */
	    return(ordermap[i].order);
	}
    }

    return 0; 
}

/*
 *	Reorder on the basis of information from ".aux" file.
 */

void Reorder()
{
int i;
int j;
int min;
struct entry* e;
int o1, o2;

    for (i = 0; i < nidents-1; i++) {
	min = i; 
	for (j = i+1; j < nidents; j++) {
	    o1 = OrderOf(identtable[  j  ]->name);
	    o2 = OrderOf(identtable[ min ]->name);

	    if (o1 < o2 ) min = j;
	}

        e = identtable[ min ];
	identtable[ min ] = identtable[ i ];
	identtable[ i ] = e;
    } 	
}

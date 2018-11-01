#include <stdio.h>
#include "Defines.h"
#include "Main.h"

extern void exit();

/*VARARGS0*/
void Error(a1,a2,a3,a4)
char* a1; char* a2; char* a3; char* a4;
{
    fprintf(stderr, "%s: ", programname);
    fprintf(stderr, a1, a2, a3, a4);
    fprintf(stderr, "\n");
    exit(1);
}

/*VARARGS0*/
void Disaster(a1,a2,a3,a4)
char* a1; char* a2; char* a3; char* a4;
{
    fprintf(stderr, "%s: ", programname);
    fprintf(stderr, " Disaster! ("); 
    fprintf(stderr, a1, a2, a3, a4);
    fprintf(stderr, ")\n");
    exit(1);
}

void Usage()
{
   printf("usage: %s -b -d -ef -g -p -s -y\n", programname);
   printf("where -b  use large title box\n");
   printf("      -d  sort by standard deviation\n"); 
   printf("      -ef[in|mm|pt] produce Encapsulated PostScript f units wide\n");
   printf("      -g  produce output suitable for GHOSTSCRIPT previever\n");
   printf("      -p  use previous scaling, shading and ordering\n");
   printf("      -s  use small title box\n");
   printf("      -y  traditional\n");
   printf("      -l  display a separate layer for each component\n");
   printf("      -C  use colours instead of different shades of gray\n");
   exit(0);
}


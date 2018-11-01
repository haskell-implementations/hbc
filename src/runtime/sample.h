#define GROUP		0
#define MODULE		1
#define PRODUCER	2
#define CONSTRUCTION	3
#define TYPE		4
#define NPROFINFO	5
 
struct hprofinfo {
    int hp_magic;
    char *hp_info[NPROFINFO];
};
#include "../mcode/magic.h"

typedef struct HPnode2 {
    Node11 n11;
    struct hprofinfo *hpinf;
} HPnode2;
typedef struct HPnode3 {
    Node12 n12;
    struct hprofinfo *hpinf;
} HPnode3;
typedef union {
    HPnode2 n2;
    HPnode3 n3;
    Nodevap nvap;
    Nodevek nvek;
} HPNode;
typedef struct HPnodefwd {
    Tag *tag;
    HPNode *forward;
    struct HPnodefwd *next;
} HPnodefwd;

extern struct hprofinfo CSYSTEM_SYSTEM_LABEL;

#define SLOP1   ((void *)(&CSYSTEM_SYSTEM_LABEL))
/*#define SLOP2   0*/

double milliseconds;
double atof();
char *ctime();
double sampleinterval;
double nextsampletime;	
int noautosample;

int grpresflag;  		/* groups are restricted	*/
int modresflag;  		/* modules are restricted	*/
int proresflag;  		/* producers are restricted	*/
int conresflag;  		/* constructors are restricted 	*/
int typresflag;  		/* types are restricted      	*/
int someresflag;		/* something is restricted	*/

int profiling;			/* heap profiling requested	   */

int hashon;			/* can be GROUP, MODULE, PRODUCER, */ 
		                /* CONSTRUCTION, TYPE    	   */
int gengcmark;			/* generate a MARK in the profile file for each gc */

FILE* hpfile;                   /* graph profile file      		*/
char *hpfilename;               /* buffer to build up name 		*/

int sampleflag;		     	/* true if sampling is taking place 	*/

struct restrlist {
    char *name;
    struct restrlist *next;
};

struct restrlist * grpres;    /* restricted groups	 */
struct restrlist * modres;    /* restricted modules	 */
struct restrlist * prores;    /* restricted producers     */
struct restrlist * conres;    /* restricted constructions */
struct restrlist * typres;    /* restricted types	 */

#ifdef SLOP2
#define HPSET2(p) ((int **)(p))[2] = (int*)SLOP1; ((int **)(p))[3] = SLOP2
#define HPSET3(p) ((int **)(p))[3] = (int*)SLOP1; ((int **)(p))[4] = SLOP2
#else
#define HPSET2(p) ((int **)(p))[2] = (int*)SLOP1
#define HPSET3(p) ((int **)(p))[3] = (int*)SLOP1
#endif

#define SAMPLEMAX 32
#define STARTINTERVAL 0.25

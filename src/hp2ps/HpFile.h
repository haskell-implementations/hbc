typedef enum {
        /* These tokens are found in ".hp" files */ 
 
	EOF_TOK,
	INTEGER_TOK,
	FLOAT_TOK,
	IDENTIFIER_TOK,
	STRING_TOK,
	BEGIN_SAMPLE_TOK,
	END_SAMPLE_TOK,
	JOB_TOK, 
	DATE_TOK,
	SAMPLE_UNIT_TOK,
	VALUE_UNIT_TOK,
	MARK_TOK,
 
	/* These extra ones are found only in ".aux" files */ 
 
	X_RANGE_TOK,
	Y_RANGE_TOK,
	ORDER_TOK,
	SHADE_TOK
} token;


struct datapoint {
    int bucket;
    float value;
};

struct chunk {
    struct chunk* next;
    short  nd;                          /* 0 .. N_CHUNK - 1 */
    struct datapoint* d;
};


struct entry {
    struct entry* next;
    struct chunk* chk;
    char*  name;
};

extern char *theident;
extern char *thestring;
extern int theinteger;
extern float thefloat;
extern int ch;
extern token thetok;
extern int linenum; 
extern int endfile;

extern char* TokenToString();

extern struct entry** identtable;
extern int nidents;

extern float *samplemap;
extern float *markmap;

extern void GetHpFile();
extern void StoreSample();
extern struct entry* MakeEntry();

extern token GetNumber();
extern void  GetIdent();
extern void  GetString();
extern int   IsIdChar();

extern char *jobstring;
extern char *datestring;
 
extern char *sampleunitstring;
extern char *valueunitstring;


#include "include.h"

#include "proto.h"

extern int yychar;
extern int yylineno;
extern char yytext[];
extern char *filename;
extern int interactive;
int wantrcurl = 0;

void
yyerror(s)
char *s;
{
    char b[1024];

    if (wantrcurl && strcmp(s,"syntax error") == 0)
	return;

    if (interactive) {
	sprintf(b, "%s ", s);
    } else {
	if(yylineno)
	    sprintf(b, "\"%s\", line %d, %s ", filename, yylineno, s);
	else
	    sprintf(b, "\"%s\", %s ", filename, s);
    }
    sprintf(b+strlen(b), "on input:");
    if( yychar >= 0400 )
	sprintf(b+strlen(b), "%s\n", &yytext[0]);
    else
	switch(yychar) {
	case '\t' : sprintf(b+strlen(b), "\\t"); break;
	case '\n' : sprintf(b+strlen(b), "\\n"); break;
        case -1   :
	case '\0' : sprintf(b+strlen(b), "<eof>"); break;
	default: sprintf(b+strlen(b), "%c", yychar); break;
	}
    sprintf(b+strlen(b), "\n");
    if (interactive)
	errmsg(b);
    else {
	fprintf(stderr, b);
	exit(1);
    }
}

void
obsolete(s)
char *s;
{
	fprintf(stderr, "Warning: Obsolete construction '%s'", s);
	if(yylineno)
		fprintf(stderr, ", line %d, \"%s\"\n", yylineno, filename);
}

%{
#include "id.h"
#include "tree.h"
#include "y.tab.h"
#include <stdio.h>
extern  FILE *fc;
#ifdef ECHO
#undef ECHO
#endif
#define ECHO 

%}
%%
";"			{ ECHO; return(SEMICOL); }
":"			{ ECHO; return(COLON); }
"<"			{ ECHO; return(STDEF); }
">"			{ ECHO; return(ENDDEF); }
"type"	 		{ ECHO; return(TYPE); }
"end"	 		{ ECHO; return(END); }
[A-Za-z][A-Za-z0-9_]*	{
				ECHO;
				yylval.uid = installid(yytext);
				return(ID);
			}
.			ECHO;
"\n"			ECHO; 
"/*"([^*]|"*"[^/]|\n)*"*/" ECHO;
"%{"([^%]|"%"[^}]|\n)*"%}" {
				ECHO;
				yytext[yyleng-2] = '\0';
				fprintf(fc, "\n%s",  &yytext[2]);
			}
%%
#ifndef yywrap
yywrap()
{
	return(1);
}
#endif

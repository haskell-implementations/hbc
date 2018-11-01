extern int yylineno;

yyerror(s)
 char *s;
{
	extern int yychar;
	extern char *yyterm[];
	extern char yytext[1];

	printf("\n%s", s);
	if(yylineno)
		printf(", line %d,", yylineno);
	printf("on input: ");
	if( yychar >= 0400 )
		printf("%s\n", &yytext[0]);
	else
		switch(yychar) {
		  case '\t' : printf("\\t\n"); return;
		  case '\n' : printf("\\n\n"); return;
		  case '\0' : printf("$end\n"); return;
		  default   : printf("%c\n", yychar); return;
		}
}

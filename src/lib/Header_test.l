/*
**	Test:	checks that the header in a file looks like this one,
**		that is:
**		/*
**		**	NAME COLON lines describing the function,
**		**	lines between /* and * / are echoed.
**		* /
*/
%{
# define	NAME	1
# define	END	2
# define	BAD	3
%}

%Start	first name descr more

%%
"/*"\n					{ BEGIN first; }
"**"\t					{ ECHO; BEGIN name; }
<name>[A-Za-z][A-Za-z0-9_]*		{ ECHO; BEGIN descr; return(NAME); }
<descr>.*\n				{ ECHO; BEGIN more; }
<more>"**"\n				{ ECHO; }
<more>"**"\t.*\n			{ ECHO; }
<more>"*/"\n				{ BEGIN 0; return(END); }
.					{ BEGIN 0; return(BAD); }
%%
#ifndef yywrap
yywrap()
{
	return(1);
}
#endif

extern	char	*rindex();

int	estat	= 0;	/* exit status */
char	funname[100];

error(f,a,b,c)
char	*f;
int	a,b,c;
{
	fflush(stdout);
	fprintf(stderr, "Headertest: ");
	fprintf(stderr, f, a, b, c);
	estat = 1;
}

main(argc, argv)
int	argc;
char	*argv[];
{
	char	*cp;
	int	l;

	if (argc < 2) {
		error("usage: test file.m ...");
		exit(1);
	}

	while (argc > 1) {
		argc--; argv++;
		if ((cp = rindex(argv[0], '.')) == 0 || strcmp(cp, ".m") != 0) {
			error("'%s' isn't a .m file\n", argv[0]);
			continue;
		}
		*cp = '\0';
		strcpy(funname, argv[0]);
		*cp = '.';
		if (freopen(argv[0], "r", stdin) == NULL) {
			error("cannot open '%s'\n", argv[0]);
			continue;
		}
		if ((l = yylex()) != NAME) {
			error("bad header \"%s\" in '%s' (see Skeleton)\n",
							yytext, argv[0]);
			continue;
		}
		if (strcmp(yytext, funname) != 0) {
			error("function-name '%s' not same as file-name '%s'\n",
							yytext, argv[0]);
			continue;
		}
		if ((l = yylex()) != END) {
			error("bad header \"%s\" in '%s' (see Skeleton)\n",
							yytext, argv[0]);
			continue;
		}
		if (argc > 0)
			printf("\n");
	}
	exit(estat);
}

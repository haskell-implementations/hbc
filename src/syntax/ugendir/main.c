#include <stdio.h>
#include "id.h"
#include "tree.h"

FILE *fh, *fc;

tree root; /* The root of the built syntax tree. */

main(argc, argv)
    int argc;
    char **argv;
{
	int i = 0;

	if(argc != 2) {
		printf("Missing input file.\n");
		exit(1);
	}

	if(freopen(argv[1], "r", stdin) == NULL) {
		printf("Cannot open %s.\n", argv[1]);
		exit(1);
	}

	while(argv[1][i+1] != 0)
		i++;
	if(! (argv[1][i-3] == '.' &&
	      argv[1][i-2] == 'u' &&
	      argv[1][i-1] == 'g' &&
	      argv[1][i]   == 'n')) {
		printf("Not a .ugn file\n");
		exit(1);
	}

	i -= 2;
	argv[1][i+1] = 0;
	argv[1][i] = 'c';
	fc = fopen(argv[1], "w");
	argv[1][i] = 'h';
	fh = fopen(argv[1], "w");

	if(yyparse() == 0) {
		/* No syntax errors. */

		fprintf(fc, "#include \"%s\"\n", argv[1]);
		gentype(root);
		exit(0);

	} else {
		/* There was a syntax error. */
		unlink(argv[1][i]);
		argv[i][i] = 'c';
		unlink(argv[1][i]);
		printf("Nothing generated.\n");
		exit(1);
	}
}


gentype(t)
   tree t;
{
		ge_typdef(t); /* Generate the .h - file. */

		/* Generate the struct definitions. */
		gs_typlist(gtdeflist(t), gtid(t));

		/* Generate the function returning the tag. */
		g_tagfun(gtid(t));

		/* Generate constructors and selectors. */
		g_consels(gtdeflist(t), gtid(t));
}

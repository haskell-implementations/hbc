BEGIN { printf("static char *clextab[] = {\n"); }
/# define/ { if (set == 0) { set = $4; }; printf("\"%s\",\n", $3);}
/#define/ { if (set == 0) { set = $3; }; printf("\"%s\",\n", $2);}
END { printf("};\nchar *clexeme(n) int n; { return n < 0 ? \"EOF\" : clextab[n-%d]; }\n", set); }

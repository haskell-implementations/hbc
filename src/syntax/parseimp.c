#include <stdio.h>
main()
{
	char buff[10000];
	int n;

	n = fread(buff, 1, sizeof buff, stdin);
	fwrite(buff+6, 1, n-50-4, stdout);
	exit(0);
}

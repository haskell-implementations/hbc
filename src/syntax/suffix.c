#include "suffix.h"

void set_suffix(f,s)
char *f,*s;
{
  int dp=strlen(f);
  while(dp>=0 && f[dp]!=s[0])
    dp--;
  if (dp<0) dp=strlen(f);
  strcpy(f+dp,s);
}

void add_o(f)
char *f;
{
#if 0
  if(loadsharelib)
    set_suffix(f,".so");
  else
#endif
    set_suffix(f,".o");
}

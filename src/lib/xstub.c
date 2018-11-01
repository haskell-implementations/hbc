#include <stdio.h>
#include "dialog.h"

void
xdocommand(wid, p2)
int wid, **p2;
{
    mkerrresp(OtherError, "No Xlib");
}

void
xgetevents(n)
int n;
{
    mkerrresp(OtherError, "No Xlib");
}

void
xmkresponse()
{
}

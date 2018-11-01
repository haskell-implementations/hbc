#include "node.h"

#if defined(sun) && !defined(SOLARIS)

#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

int connect PROTO((int, struct sockaddr *, int));

static void
fdreopen(fd, mode, file)
int fd;
char *mode;
FILE *file;
{
	file->_file = fd;
}

/*
 * Handle communication with NeWS.
 *
 * Original code by Andy Dwelly @ ECRC
 */
FILE *
grab_news()
{
    char *ev;
    struct sockaddr_in server;
    struct hostent *nhp,*gethostbyname();
    char *ipaddr,*ipport,*hostname;
    int ps;

    /* Get the IP address, port and hostname from */
    /* the environment variable NEWSSERVER        */
    ev = getenv("NEWSSERVER");
    if (ev == NULL) {
	fprintf(stderr,"Unable to locate the NeWS Server\n");
	finish(1);
    }

    /* Parse the input */
    ipaddr = ev;
    if (!(ev = strchr(ev, '.'))) {
	fprintf(stderr, "Malformed NEWSSERVER address\n");
	finish(1);
    }
    *ev++ = 0;
    ipport = ev;
    if (!(ev = strchr(ev, ';'))) {
	fprintf(stderr, "Malformed NEWSSERVER address\n");
	finish(1);
    }
    *ev++ = 0;
    hostname = ev;

    /* Create a pset for communications */
    ps = socket(AF_INET,SOCK_STREAM,0);

    if (ps < 0) {
	fprintf(stderr,"Unable to open a socket\n");
	finish(1);
    }

    /* Connect to NeWS server */
    server.sin_family = AF_INET;
    nhp = gethostbyname(hostname);
    if (nhp == 0) {
	fprintf(stderr,"%s : Unknown host\n", hostname);
	finish(1);
    }

    bcopy(nhp->h_addr, (char *)&server.sin_addr, nhp->h_length);
    server.sin_port = htons(atoi(ipport));

    if (connect(ps,(struct sockaddr *)&server,sizeof(server)) < 0) {
	fprintf(stderr,"Unable to communicate with NeWS\n");
	finish(1);
    }

    /* Make this stdin and stdout */
    fdreopen(ps, "r", stdin);
    return fdopen(ps, "w");
}

#else /* sun */
FILE *
grab_news()
{
    return 0;
}
#endif /* sun */

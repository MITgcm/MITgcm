/*
//BOP
// !ROUTINE: sigreg
// !INTERFACE:
   sigreg()

// !DESCRIPTION:
// Register a signal handler

//EOP

*/
#ifdef HAVE_SIGREG
/*  Here, we get the definition of the FC_NAMEMANGLE() macro. */
#include "FC_NAMEMANGLE.h"

/* #define FC_NAMEMANGLE(X) X ## _ */

#ifdef HAVE_SIGREG
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <ucontext.h>

int * sigreg_ip;

static void killhandler(
    unsigned int sn, siginfo_t  si, struct ucontext *sc )
{
    *sigreg_ip = *sigreg_ip + 1;
    return;
}
#endif

/* int main( int argc, char ** argv ) */
void FC_NAMEMANGLE(sigreg) (int * aip)
{
#ifdef HAVE_SIGREG
    struct sigaction s;
    sigreg_ip = aip;
    s.sa_flags = SA_SIGINFO;
    s.sa_sigaction = (void *)killhandler;
    if(sigaction (SIGTERM,&s,(struct sigaction *)NULL)) {
	printf("Sigaction returned error = %d\n", errno);
	exit(0);
    }
#endif
    return;
}
#endif


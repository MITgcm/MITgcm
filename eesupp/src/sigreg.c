/*
 * $Header: /u/gcmpack/MITgcm/eesupp/src/sigreg.c,v 1.1 2005/12/03 08:30:32 edhill Exp $
 * $Name:  $

//BOP
// !ROUTINE: sigreg
// !INTERFACE:
   sigreg()

// !DESCRIPTION:
// Register a signal handler

//EOP

*/

/*  Here, we get the definition of the FC_NAMEMANGLE() macro. */
#include "FC_NAMEMANGLE.h"

/* #define FC_NAMEMANGLE(X) X ## _ */

#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <ucontext.h>

int * ip;

static void killhandler(
    unsigned int sn, siginfo_t  si, struct ucontext *sc )
{
    *ip = *ip + 1;
    return;
}

/* int main( int argc, char ** argv ) */
void FC_NAMEMANGLE(sigreg) (int * aip)
{
    ip = aip;
    struct sigaction s;
    s.sa_flags = SA_SIGINFO;
    s.sa_sigaction = (void *)killhandler;
    if(sigaction (SIGTERM,&s,(struct sigaction *)NULL)) {
	printf("Sigaction returned error = %d\n", errno);
	exit(0);
    }
    return;
}



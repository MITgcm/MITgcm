/*
 * $Header: /u/gcmpack/MITgcm/eesupp/src/setrlstk.c,v 1.1 2005/09/11 18:52:26 edhill Exp $
 * $Name:  $

//BOP
// !ROUTINE: setrlstk
// !INTERFACE:
   setrlstk()

// !DESCRIPTION:
// Unlimit the size of the stack using setrlimit().

//EOP

*/

/*  Here, we get the definition of the FC_NAMEMANGLE() macro. */
#include "FC_NAMEMANGLE.h"

/* #define FC_NAMEMANGLE(X) X ## _ */

#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

/* int main( int argc, char ** argv ) */
void FC_NAMEMANGLE(setrlstk) ()
{
    struct rlimit rls;

    rls.rlim_cur = RLIM_INFINITY;
    rls.rlim_max = RLIM_INFINITY;
    setrlimit(RLIMIT_STACK, &rls);
    /*  system("ulimit -a");  */
    return;
}



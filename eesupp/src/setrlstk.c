/*
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
#ifdef HAVE_SETRLSTK
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#endif

/* int main( int argc, char ** argv ) */
void FC_NAMEMANGLE(setrlstk) ()
{
#ifdef HAVE_SETRLSTK
    struct rlimit rls;

    rls.rlim_cur = RLIM_INFINITY;
    rls.rlim_max = RLIM_INFINITY;
    setrlimit(RLIMIT_STACK, &rls);
    /*  system("ulimit -a");  */
#endif
    return;
}



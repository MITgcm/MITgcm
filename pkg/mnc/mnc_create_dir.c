/*************************************************  -*- mode: C -*-
 * $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_create_dir.c,v 1.1 2004/03/22 05:10:10 edhill Exp $
 * $Name:  $
 */

/*  Here, we get the definition of the FC_NAMEMANGLE() macro. */
#include "FC_NAMEMANGLE.h"

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>
void FC_NAMEMANGLE(mnccdir) ( int *iodate, int *ioseq )
{
    char name[512];
    struct tm * tmp;
    time_t tt;
    int i;
    int iyyyymmdd, iseq;
    int ierr = 0;
    mode_t mode = 0x0;

    tt = time(&tt);
    tmp = localtime(&tt);
    iyyyymmdd = tmp->tm_mday + 100*(tmp->tm_mon + 100*(1900+tmp->tm_year));
    *iodate = iyyyymmdd;
    for (i=1; i<10000; i++) {
	sprintf(&name[0], "mnc_%08d_%04d\0", iyyyymmdd, i);
	mode = S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH;
	if (mkdir(&name[0], mode) == 0) {
	    break;
	}
    }
    *ioseq = i;
}



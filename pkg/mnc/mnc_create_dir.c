/*************************************************  -*- mode: C -*-
 * $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_create_dir.c,v 1.4 2006/03/10 05:50:23 edhill Exp $
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
#include <string.h>
void FC_NAMEMANGLE(mnccdir) ( int *nienc, int *ienc, int *idate )
{
    char name[512], dname[512];
    struct tm * tmp;
    time_t tt;
    int i, j, n, ntot, iyyyymmdd;
    mode_t mode = 0x0;
    char * cenc = 
	{"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.,+-=/~\0"};
    int ncenc = 70;

    n = ((*nienc) > 500) ? 500 : (*nienc);
    tt = time(&tt);
    tmp = localtime(&tt);
    iyyyymmdd = tmp->tm_mday 
	+ 100*(tmp->tm_mon + 1 + 100*(1900+tmp->tm_year));
    for (i=0; i<n; i++) {
	/* printf("ienc = %d, %c\n", ienc[i], cenc[ienc[i] - 1]); */
	name[i] = cenc[ienc[i] - 1];
    }
    name[n] = '\0';
    /* printf("name = \"%s\", n = %d\n", name, n); */
    for (i=1; i<10000; i++) {
	if (*idate == 1) 
	    snprintf(dname, 500, "%s%08d_%04d\0", name, iyyyymmdd, i);
	else
	    snprintf(dname, 500, "%s%04d\0", name, i);
	mode = S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH;
	if (mkdir(dname, mode) == 0) {
	    break;
	}
    }
    n = (int)(strlen(dname));
    /* printf("dname = \"%s\", n = %d\n", dname, n); */
    ntot = 0;
    for (i=0; i<n; i++) {
	for (j=0; j<ncenc; j++) {
	    if (cenc[j] == dname[i]) {
		ienc[ntot] = j + 1;
		ntot++;
		break;
	    }
	}
    }
    *nienc = ntot;
}



/*************************************************  -*- mode: C -*-
 * $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cw_fsize.c,v 1.1 2004/10/21 15:40:38 edhill Exp $
 * $Name:  $
 */

/*  Here, we get the definition of the FC_NAMEMANGLE() macro. */
#include "FC_NAMEMANGLE.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

void FC_NAMEMANGLE(mncfsize) ( int *nienc, int *ienc, int *nbyte )
{
    char name[512];
    struct stat astat;
    int i, j, n;
    char * cenc = 
	{"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.,+-=/~\0"};
    int ncenc = 70;

    n = ((*nienc) > 200) ? 200 : (*nienc);
    for (i=0; i<n; i++) {
	/* printf("ienc = %d, %c\n", ienc[i], cenc[ienc[i] - 1]); */
	name[i] = cenc[ienc[i] - 1];
    }
    name[n] = '\0';
    /* printf("name = \"%s\", n = %d\n", name, n); */

    /*  At this point we have the file name and would like to
     *  determine the file size. */
    if (! stat(name, &astat))
	*nbyte = (int)(astat.st_size);
    else
	*nbyte = -1;
}


